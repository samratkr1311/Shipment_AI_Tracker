CLASS lhc_Shipment DEFINITION INHERITING FROM cl_abap_behavior_handler.

  PUBLIC SECTION.

    DATA : lv_orgin TYPE zcountry_coord-latitude,
           lv_dest  TYPE zcountry_coord-longitude.


  PRIVATE SECTION.

    METHODS get_instance_authorizations FOR INSTANCE AUTHORIZATION
      IMPORTING keys REQUEST requested_authorizations FOR Shipment RESULT result.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR Shipment RESULT result.

    METHODS ship_event_type FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Shipment~ship_event_type.

    METHODS ai_preview FOR MODIFY
      IMPORTING keys FOR ACTION Shipment~ai_preview.

    METHODS calculate_delay_risk FOR DETERMINE ON MODIFY
      IMPORTING keys FOR Shipment~calculate_delay_risk.
    METHODS ship_email FOR MODIFY
      IMPORTING keys FOR ACTION Shipment~ship_email.

    METHODS earlynumbering_create FOR NUMBERING
      IMPORTING entities FOR CREATE Shipment.

    METHODS earlynumbering_cba_Trackereven FOR NUMBERING
      IMPORTING entities FOR CREATE Shipment\_Trackerevent.

    METHODS earlynumbering_cba_Prediction FOR NUMBERING
      IMPORTING entites FOR CREATE Shipment\_Prediction.

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

    DATA:
      lt_update TYPE TABLE FOR UPDATE zi_shipment_track,
      "      lt_result TYPE TABLE FOR ACTION RESULT zi_shipment_track~ai_preview,
      lt_pred   TYPE TABLE FOR CREATE zi_shipment_track\_Prediction.

    READ ENTITIES OF zi_shipment_track
      IN LOCAL MODE
      ENTITY Shipment
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_shipments).

    LOOP AT lt_shipments ASSIGNING FIELD-SYMBOL(<s>).

      DATA lv_summary TYPE char256.

      CASE <s>-CurrentStatus.
        WHEN 'DELAY'.
          lv_summary = |Predicted Delay: 3 Days. Check transport conditions.|.
        WHEN 'OnTime'.
          lv_summary = |Shipment is on time. No delay expected.|.
        WHEN OTHERS.
          lv_summary = |AI Prediction not available yet.|.
      ENDCASE.

      " 1️⃣ Update shipment
      APPEND VALUE #(
        %tky      = <s>-%tky
        AiSummary = lv_summary
      ) TO lt_update.

*      APPEND VALUE #(
*        %tky = <s>-%tky
*        %is_draft = abap_true
*      ) TO lt_result.

    ENDLOOP.

    " 3️⃣ Update shipment AI summary
    IF lt_update IS NOT INITIAL.
      MODIFY ENTITIES OF zi_shipment_track
        IN LOCAL MODE
        ENTITY Shipment
        UPDATE FIELDS ( AiSummary )
        WITH lt_update
        FAILED   DATA(lt_failed)
        REPORTED DATA(lt_reported).
    ENDIF.


*    DATA(lo_api) = NEW zcl_country_geo_util( ).
*    DATA ls_country_coord TYPE zcountry_coord.
*    DATA ls_country_coord2 TYPE zcountry_coord.
*
*
*    ls_country_coord = lo_api->fetch_store_cordinates(
*                         iv_country_iso  = 'IN'
*                         iv_country_name =  'INDIA'
*                       ).
*
*    ls_country_coord2 = lo_api->fetch_store_cordinates(
*                      iv_country_iso  = 'US'
*                      iv_country_name =  'USA'
*                    ).
*
*
*           lv_orgin = ls_country_coord-latitude.
*           lv_dest  = ls_country_coord2-longitude.



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


  METHOD earlynumbering_cba_prediction.

*    DATA : lv_prompt   TYPE string,
*           lo_http     TYPE REF TO if_http_destination,
*           lo_client   TYPE REF TO if_web_http_client,
*           lo_request  TYPE REF TO if_web_http_request,
*           lo_response TYPE REF TO if_web_http_response,
*           lv_output   TYPE string,
*           lv_response TYPE string.
*
*
*    TYPES: BEGIN OF ty_message,
*             role    TYPE string,
*             content TYPE string,
*           END OF ty_message.
*
*    TYPES: BEGIN OF ty_choice,
*             index   TYPE i,
*             message TYPE ty_message,
*           END OF ty_choice.
*
*    TYPES: tt_choice TYPE STANDARD TABLE OF ty_choice WITH DEFAULT KEY.
*
*    DATA: lv_json    TYPE string,
*          lv_ai_text TYPE string.
*
*    TYPES: BEGIN OF ty_response,
*             id      TYPE string,
*             object  TYPE string,
*             created TYPE i,
*             model   TYPE string,
*             choices TYPE tt_choice,
*           END OF ty_response.
*
*    DATA: ls_response TYPE ty_response.
*    DATA: lt_update2 TYPE TABLE FOR UPDATE zi_predctionlog.
*
*
***********************api call*********************

    DATA:lt_track_event TYPE TABLE FOR MAPPED EARLY zi_predctionlog,
         ls_track_event LIKE LINE OF lt_track_event,
         lr_need_tab    TYPE REF TO data,
         lv_curr_num    TYPE n LENGTH 10.

    FIELD-SYMBOLS:
      <lt_need_number> TYPE STANDARD TABLE,
      <ls_need_number> TYPE any.


    LOOP AT entites ASSIGNING FIELD-SYMBOL(<fs>).

      IF lr_need_tab IS INITIAL.

        CREATE DATA lr_need_tab LIKE <fs>-%target.
        ASSIGN lr_need_tab->* TO <lt_need_number>.
      ENDIF..

      LOOP AT <fs>-%target ASSIGNING FIELD-SYMBOL(<ls_target>).



        IF <ls_target>-RequestId IS INITIAL.

          APPEND <ls_target> TO <lt_need_number>.
        ELSE.

          APPEND VALUE #( %cid = <ls_target>-%cid
                          %key = <ls_target>-%key )  TO mapped-prediction.
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
        <cid>          TYPE any,
        <is_draft>     TYPE any,
        <aiactiontype> TYPE any,
        <inputsnap>    TYPE any,
        <airesponse>   TYPE any,
        <calledat>     TYPE any.



      ASSIGN COMPONENT '%cid'       OF STRUCTURE <ls_need_number> TO <cid>.
      ASSIGN COMPONENT '%is_draft'  OF STRUCTURE <ls_need_number> TO <is_draft>.
      ASSIGN COMPONENT 'aiactiontype'     OF STRUCTURE <ls_need_number> TO <aiactiontype>.
      ASSIGN COMPONENT 'inputsnap'  OF STRUCTURE <ls_need_number> TO <inputsnap>.
      ASSIGN COMPONENT 'airesponse' OF STRUCTURE <ls_need_number> TO <airesponse>.
      ASSIGN COMPONENT 'calledat'  OF STRUCTURE <ls_need_number> TO <calledat>.


      IF <cid> IS NOT ASSIGNED OR <is_draft> IS NOT ASSIGNED.
        CONTINUE. " or report/failed for safety
      ENDIF.

      " Build mapped row for CHILD with final key
      ls_track_event = VALUE #(
        %cid       = <cid>
        %is_draft  = <is_draft>
        RequestId        = lv_curr_num
      ).


      APPEND ls_track_event TO mapped-prediction.


    ENDLOOP.
  ENDMETHOD.

  METHOD ship_email.

    DATA:
      lv_prompt   TYPE string,
      lv_ai_text  TYPE string,
      lv_json     TYPE string,
      lo_http     TYPE REF TO if_http_destination,
      lo_client   TYPE REF TO if_web_http_client,
      lo_request  TYPE REF TO if_web_http_request,
      lo_response TYPE REF TO if_web_http_response.

    "--------------------------------------------------
    " 1️⃣ Read ROOT Shipments
    "--------------------------------------------------
    READ ENTITIES OF zi_shipment_track
      IN LOCAL MODE
      ENTITY Shipment
      ALL FIELDS
      WITH CORRESPONDING #( keys )
      RESULT DATA(lt_shipments).

    IF lt_shipments IS INITIAL.
      RETURN.
    ENDIF.

    "--------------------------------------------------
    " 2️⃣ Read CHILD Predictions via Association
    "--------------------------------------------------
    READ ENTITIES OF zi_shipment_track
      IN LOCAL MODE
      ENTITY Shipment
      BY \_Prediction
      ALL FIELDS
      WITH VALUE #(
        FOR ls_ship IN lt_shipments
        ( %tky = ls_ship-%tky )
      )
      RESULT DATA(lt_predictions).

    IF lt_predictions IS INITIAL.
      RETURN.
    ENDIF.

    "--------------------------------------------------
    " 3️⃣ Fetch Coordinates (Trial safe)
    "--------------------------------------------------
    DATA(lo_api) = NEW zcl_country_geo_util( ).

    DATA(ls_org)  = lo_api->fetch_store_cordinates(
                      iv_country_iso  = 'IN'
                      iv_country_name = 'INDIA' ).

    DATA(ls_dest) = lo_api->fetch_store_cordinates(
                      iv_country_iso  = 'US'
                      iv_country_name = 'USA' ).

    "--------------------------------------------------
    " 4️⃣ Loop Shipment + Prediction
    "--------------------------------------------------
    LOOP AT lt_shipments INTO DATA(ls_ship2).

      READ TABLE lt_predictions INTO DATA(ls_pred)
        WITH KEY ShipmentId = ls_ship2-ShipmentId.

      IF sy-subrc <> 0.
        CONTINUE.
      ENDIF.

      "--------------------------------------------------
      " 5️⃣ Build Prompt
      "--------------------------------------------------
      lv_prompt = |Generate a professional shipment update email.\n|.
      lv_prompt &&= |Shipment Id: { ls_ship2-ShipmentId }\n|.
      lv_prompt &&= |Origin: { ls_ship2-OriginLocation }\n|.
      lv_prompt &&= |Destination: { ls_ship2-DestinationLocation }\n|.
      lv_prompt &&= |Shipment Type: { ls_ship2-ShipmentType }\n|.
      lv_prompt &&= |Origin Coordinates: { ls_org-latitude }, { ls_org-longitude }\n|.
      lv_prompt &&= |Destination Coordinates: { ls_dest-latitude }, { ls_dest-longitude }\n|.
      lv_prompt &&= |Last Updated: { ls_ship2-UpdatedAt }\n|.
      lv_prompt &&= |Keep it concise and professional.|.

      REPLACE ALL OCCURRENCES OF '"'  IN lv_prompt WITH '\"'.
      REPLACE ALL OCCURRENCES OF '\'  IN lv_prompt WITH '\\'.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline
        IN lv_prompt WITH '\n'.

      "--------------------------------------------------
      " 6️⃣ OpenAI API Call
      "--------------------------------------------------
      lo_http = cl_http_destination_provider=>create_by_url(
                  i_url = 'https://api.openai.com/v1/chat/completions' ).

      lo_client  = cl_web_http_client_manager=>create_by_http_destination( lo_http ).
      lo_request = lo_client->get_http_request( ).

      lo_request->set_header_fields( VALUE #(
        ( name = 'Content-Type'  value = 'application/json' )
        ( name = 'Authorization' value = '***' )
      ) ).

      DATA(lv_body) =
        '{ "model":"gpt-4o-mini", "messages":[{' &&
        '"role":"user","content":"' && lv_prompt && '"}]}'.

      lo_request->set_text( lv_body ).

      lo_response = lo_client->execute( if_web_http_client=>post ).
      lv_json = lo_response->get_text( ).

      "--------------------------------------------------
      " 7️⃣ Parse Response
      "--------------------------------------------------
      TYPES: BEGIN OF ty_msg,
               role    TYPE string,
               content TYPE string,
             END OF ty_msg.

      TYPES: BEGIN OF ty_choice,
               message TYPE ty_msg,
             END OF ty_choice.

      TYPES: BEGIN OF ty_resp,
               choices TYPE STANDARD TABLE OF ty_choice WITH DEFAULT KEY,
             END OF ty_resp.

      DATA ls_resp TYPE ty_resp.

      /ui2/cl_json=>deserialize(
        EXPORTING json = lv_json
        CHANGING  data = ls_resp ).

      lv_ai_text = ls_resp-choices[ 1 ]-message-content.

      "--------------------------------------------------
      " 8️⃣ Update CHILD (Prediction)
      "--------------------------------------------------
      MODIFY ENTITIES OF zi_shipment_track
        IN LOCAL MODE
        ENTITY Prediction
        UPDATE FIELDS ( airesponse calledat )
        WITH VALUE #(
          ( %tky        = ls_pred-%tky
            airesponse = lv_ai_text )
        ).

    ENDLOOP.

  ENDMETHOD.


ENDCLASS.
