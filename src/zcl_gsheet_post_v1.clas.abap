CLASS zcl_gsheet_post_v1 DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*    INTERFACES if_oo_adt_classrun .

    TYPES tt_types TYPE SORTED TABLE OF zshipment_header
                               WITH UNIQUE KEY shipment_id.

    METHODS post_to_gsheet
      IMPORTING
        lt_ship TYPE ANY TABLE
       EXPORTING
        ev_status_code   TYPE i
        ev_response_text TYPE string
        ev_error_text    TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_gsheet_post_v1 IMPLEMENTATION.




  METHOD post_to_gsheet.


**********Mapping Table
      DATA lt_map TYPE /ui2/cl_json=>name_mappings.

    lt_map = VALUE #(
      ( abap = 'SHIPMENTID'           json = 'shipment_id' )
      ( abap = 'SHIPMENTTYPE'         json = 'shipment_type' )
      ( abap = 'ORIGINLOCATION'       json = 'origin_location' )
      ( abap = 'DESTINATIONLOCATION'  json = 'destination_location' )
      ( abap = 'DISTANCEKM'           json = 'distance_km' )
      ( abap = 'CURRENTSTATUS'        json = 'current_status' )
      ( abap = 'ETADAYS'              json = 'eta_days' )
      ( abap = 'DELAYHOURS'           json = 'delay_hours' )
      ( abap = 'DELAYRISK'            json = 'delay_risk' )
      ( abap = 'DELAYRISKCRIT'        json = 'delay_risk_crit' )
      ( abap = 'AISUMMARY'            json = 'ai_summary' )
      ( abap = 'CREATEBY'             json = 'create_by' )
      ( abap = 'CREATEDAT'            json = 'created_at' )
      ( abap = 'UPDATEDAT'            json = 'updated_at' )
    ).


**********Mapping Table

    DATA : lo_dest   TYPE REF TO if_http_destination,
           lo_client TYPE REF TO if_web_http_client.


    "1. Destination tells where + how to connect

    TRY.
        cl_http_destination_provider=>create_by_url(
          EXPORTING
            i_url              = 'https://script.google.com/macros/s/AKfycbxMXxJ1kGV7nV5prUfIpUd3LZbKsZ81iQhZBlrdrX2RtwxcLgEJ9-PiKPQMMRtM7gHG/exec'
          RECEIVING
            r_http_destination = lo_dest
        ).
      CATCH cx_http_dest_provider_error.
        "handle exception
    ENDTRY.

    "2. HTTP client actually makes the call.
*        1) CL_WEB_HTTP_CLIENT_MANAGER
*    ✅ Use for:
*
*    Creating + managing HTTP clients in a structured way.
*
*    When to use:
*
*    When you want to call an external REST API (Google Sheets, CPI endpoint, 3rd party API) and you want proper lifecycle handling.

    TRY.
        cl_web_http_client_manager=>create_by_http_destination(
          EXPORTING
            i_destination = lo_dest
          RECEIVING
            r_client      = lo_client
        ).
      CATCH cx_web_http_client_error.
        "handle exception
    ENDTRY.
*     CATCH cx_web_http_client_error.




*
*  3.1. lo_client = WhatsApp app " step 2  it is
*
*        lo_request = the message box where you type " step 3
*
*        execute() = click Send
*
*        response = reply you receive


    lo_client->get_http_request(
      RECEIVING
        r_http_request = DATA(lo_request)
    ).

* 3.2 Want to call Client API using POST (Google Sheet insert)

    TRY.
        DATA(lo_client_call) = if_web_http_client=>post.
      CATCH cx_web_http_client_error.
        "handle exception
    ENDTRY.


*3.3 Set headers (important)
    TRY.
        lo_request->set_header_fields(
           VALUE #(
           (   name = 'Content-Type' value = 'application/json' )
           ( name = 'Accept'       value = 'application/json' )
           )
            ).
      CATCH cx_web_message_error.

    ENDTRY.


* Set Jason Body

*    DATA: lt_ship TYPE STANDARD TABLE OF zi_shipment_track.
*
*    SELECT * FROM zi_shipment_track
*      INTO TABLE @lt_ship
*      UP TO 50 ROWS.

    DATA(lv_json) = /ui2/cl_json=>serialize(
                      data         =   lt_ship
                  name_mappings    = lt_map
                   compress        = abap_true
                    ).


*    DATA(lv_json) = `{"shipment_id":"SHP002","status":"DELIVERED"}`.

    TRY.
        lo_request->set_text(
          EXPORTING
            i_text   =  lv_json
*    i_offset = 0
*    i_length = -1
*  RECEIVING
*    r_value  =
        ).
      CATCH cx_web_message_error.
    ENDTRY.

*Excuting Post method.

    TRY.
        DATA(lo_out) = lo_client->execute(
                         EXPORTING
                           i_method      = lo_client_call
*                   i_timeout     = 0
*                   i_request_sse = abap_false
*                 IMPORTING
*                   e_sse_enabled =
                       ).
      CATCH cx_web_http_client_error.
        "handle exception
    ENDTRY.
*               CATCH cx_web_http_client_error.

  ENDMETHOD.
ENDCLASS.
