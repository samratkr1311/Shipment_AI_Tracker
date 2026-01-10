CLASS lhc_Shipment DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Shipment RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Shipment RESULT result.

    METHODS ship_event_type FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Shipment~ship_event_type.

    METHODS ai_preview FOR MODIFY
      IMPORTING keys FOR ACTION Shipment~ai_preview RESULT result.

    METHODS calculate_delay_risk FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Shipment~calculate_delay_risk.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE Shipment.

    METHODS earlynumbering_cba_Trackereven FOR NUMBERING
      IMPORTING entities FOR CREATE Shipment\_Trackerevent.

ENDCLASS.

CLASS lhc_Shipment IMPLEMENTATION.

  METHOD get_instance_authorizations.
  ENDMETHOD.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD earlynumbering_create.

    DATA:lt_travel_tech_m TYPE TABLE FOR MAPPED EARLY zi_shipment_track,
         ls_travel_tech_m LIKE LINE OF lt_travel_tech_m.

    DATA:
      lt_need_number LIKE entities,
      lv_curr_num    TYPE n LENGTH 10.

    " 1️⃣ Handle all instances explicitly
    LOOP AT entities INTO DATA(ls_entity).

      IF ls_entity-ShipmentId IS INITIAL.
        APPEND ls_entity TO lt_need_number.
      ELSE.
        " Already numbered → MUST return mapped
        APPEND VALUE #(
          %cid       = ls_entity-%cid
          %key       = ls_entity-%key
        ) TO mapped-shipment.
      ENDIF.

    ENDLOOP.

    " 2️⃣ If nothing needs numbering, we are done
    IF lt_need_number IS INITIAL.
      RETURN.
    ENDIF.

    " 3️⃣ Get numbers
    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            object            = '/DMO/TRV_M'
            nr_range_nr       = '01'
            quantity          = CONV #( lines( lt_need_number ) )
          IMPORTING
            number            = DATA(lv_latest_num)
            returned_quantity = DATA(lv_qty)
        ).
      CATCH cx_number_ranges INTO DATA(lo_error).

        LOOP AT lt_need_number INTO DATA(ls_fail).
          APPEND VALUE #(
            %cid = ls_fail-%cid
            %key = ls_fail-%key
          ) TO failed-shipment.

          APPEND VALUE #(
            %cid = ls_fail-%cid
            %key = ls_fail-%key
            %msg = lo_error
          ) TO reported-shipment.
        ENDLOOP.

        RETURN.
    ENDTRY.

    ASSERT lv_qty = lines( lt_need_number ).

    lv_curr_num = lv_latest_num - lv_qty.

    " 4️⃣ Assign numbers
    LOOP AT lt_need_number INTO DATA(ls_num).

      lv_curr_num = lv_curr_num + 1.

      ls_travel_tech_m = VALUE #( %cid = ls_num-%cid
                                  %is_draft = 1
                                   ShipmentId = lv_curr_num  ).


      APPEND ls_travel_tech_m  TO mapped-shipment.

    ENDLOOP.


  ENDMETHOD.

  METHOD earlynumbering_cba_Trackereven.

    DATA:lt_track_event TYPE TABLE FOR MAPPED EARLY zi_ship_trackevent,
         ls_track_event LIKE LINE OF lt_track_event,
         lr_need_tab    TYPE REF TO data,
         lv_curr_num    TYPE n LENGTH 10.

    FIELD-SYMBOLS:
      <lt_need_number> TYPE STANDARD TABLE,
      <ls_need_number> TYPE any.


    LOOP AT entities ASSIGNING  FIELD-SYMBOL(<fs>).

      IF lr_need_tab IS INITIAL.

        CREATE DATA lr_need_tab LIKE <fs>-%target.
        ASSIGN lr_need_tab->* TO <lt_need_number>.
      ENDIF..

      LOOP AT <fs>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).

        IF <ls_target>-EventId IS INITIAL.

          APPEND <ls_target> TO <lt_need_number>.
        ELSE.

          APPEND VALUE #( %cid = <ls_target>-%cid
                          %key = <ls_target>-%key )  TO mapped-tracker.
        ENDIF.

      ENDLOOP.

    ENDLOOP.


    IF <lt_need_number> IS INITIAL.

      RETURN.

    ENDIF.


*Get Number

    TRY.
        cl_numberrange_runtime=>number_get(
          EXPORTING
            object            = '/DMO/TRV_M'
            nr_range_nr       = '01'
            quantity          = CONV #( lines( <lt_need_number> ) )
          IMPORTING
            number            = DATA(lv_latest_num)
            returned_quantity = DATA(lv_qty)
        ).
      CATCH cx_number_ranges INTO DATA(lo_error).

        RETURN.
    ENDTRY.

    ASSERT lv_qty = lines( <lt_need_number> ).

    lv_curr_num = lv_latest_num - lv_qty.
    " 4️⃣ Assign numbers
    LOOP AT <lt_need_number> ASSIGNING <ls_need_number>.

      lv_curr_num = lv_curr_num + 1.

      FIELD-SYMBOLS:
        <cid>        TYPE any,
        <is_draft>   TYPE any,
        <shipmentid> TYPE any,
        <eventtype>  TYPE any,
        <eventloc>   TYPE any,
        <eventtime>  TYPE any.



      ASSIGN COMPONENT '%cid'       OF STRUCTURE <ls_need_number> TO <cid>.
      ASSIGN COMPONENT '%is_draft'  OF STRUCTURE <ls_need_number> TO <is_draft>.
      ASSIGN COMPONENT 'Eventtime'     OF STRUCTURE <ls_need_number> TO <eventtime>.
      ASSIGN COMPONENT 'EventType'  OF STRUCTURE <ls_need_number> TO <eventtype>.
      ASSIGN COMPONENT 'EventLocation' OF STRUCTURE <ls_need_number> TO <eventloc>.
      ASSIGN COMPONENT 'EventTime'  OF STRUCTURE <ls_need_number> TO <eventtime>.


      IF <cid> IS NOT ASSIGNED OR <is_draft> IS NOT ASSIGNED.
        CONTINUE. " or report/failed for safety
      ENDIF.

      " Build mapped row for CHILD with final key
      ls_track_event = VALUE #(
        %cid       = <cid>
        %is_draft  = <is_draft>
        EventId        = lv_curr_num
      ).

      APPEND ls_track_event TO mapped-tracker.

    ENDLOOP.


  ENDMETHOD.

  METHOD ship_event_type.

  ENDMETHOD.

  METHOD ai_preview.

  ENDMETHOD.

  METHOD calculate_delay_risk.

    DATA : lt_update   TYPE TABLE FOR UPDATE zi_shipment_track,
           lv_new_risk TYPE zi_shipment_track-delayrisk.

    READ ENTITIES OF zi_shipment_track
      IN LOCAL MODE
      ENTITY Shipment
      FIELDS ( DelayHours DelayRisk )
      WITH CORRESPONDING #( keys )
      RESULT DATA(shipments).

    LOOP AT shipments ASSIGNING FIELD-SYMBOL(<s>).

      DATA(lv_risk) = <s>-DelayRisk.
      DATA(lv_crit) = <s>-DelayRiskCrit.

      IF <s>-DelayHours IS INITIAL OR <s>-DelayHours <= 8.
        lv_risk = 'ON_TRACK'.
        lv_crit = 3. " GREEN
      ELSEIF <s>-DelayHours <= 24.
        lv_risk = 'AT_RISK'.
        lv_crit = 2. " YELLOW
      ELSE.
        lv_risk = 'DELAYED'.
        lv_crit = 1. " RED
      ENDIF.

      IF lv_risk <> <s>-DelayRisk OR lv_crit <> <s>-DelayRiskCrit.
        APPEND VALUE #(
          %tky          = <s>-%tky
          DelayRisk     = lv_risk
          DelayRiskCrit = lv_crit
        ) TO lt_update.
      ENDIF.

    ENDLOOP.

    " 3️⃣ Perform update only when needed
    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF zi_shipment_track
        IN LOCAL MODE
        ENTITY Shipment
        UPDATE FIELDS ( DelayRisk )
        WITH lt_update
        FAILED   DATA(lt_failed)
        REPORTED DATA(lt_reported).
    ENDIF.

  ENDMETHOD.


ENDCLASS.
