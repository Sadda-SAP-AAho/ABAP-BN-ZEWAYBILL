CLASS zcl_http_ewaybillbyirn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_EWAYBILLBYIRN IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>post ).
*        DATA token_url TYPE string .
*        DATA lv_token TYPE string.
*        DATA lv_client TYPE REF TO if_web_http_client.
*        DATA req TYPE REF TO if_web_http_client.                            12/03/2025
*        DATA irn_url TYPE string .
*        DATA lv_client2 TYPE REF TO if_web_http_client.
*        DATA req3 TYPE REF TO if_web_http_client.
*        irn_url = 'https://apimsapintegration.azure-api.net/api/v1/gen-ewb-by-irn/' .
*

        TRY.
*            DATA(dest2) = cl_http_destination_provider=>create_by_url( irn_url ).                     12/03/2025
*            lv_client2 = cl_web_http_client_manager=>create_by_http_destination( dest2 ).

          CATCH cx_static_check INTO DATA(lv_cx_static_check2).
            response->set_text( lv_cx_static_check2->get_longtext( ) ).
        ENDTRY.
        TYPES: BEGIN OF ty_json,
                 companycode TYPE string,
                 document    TYPE string,
               END OF ty_json.

        DATA: lv_json1 TYPE ty_json.

        CALL METHOD /ui2/cl_json=>deserialize
          EXPORTING
            json = request->get_text( )
          CHANGING
            data = lv_json1.

*        DATA: lv_bukrs TYPE ztable_irn-bukrs.
*        DATA: lv_invoice TYPE ztable_irn-billingdocno.          12/03/2025
*        lv_bukrs = lv_json1-companycode.
*        lv_invoice = lv_json1-document.

*  12/03/2025        DATA(get_payload) = zcl_ewaybillbyirn_generation=>generated_ewaybillbyirn( companycode = lv_bukrs document = lv_invoice ).
*
*        response->set_text( get_payload ).

*  12/03/2025       DATA(req4) = lv_client2->get_http_request( ).

*        lv_token = |JWT { lv_token }|.
*        req4->set_header_field(    12/03/2025
*          EXPORTING                 12/03/2025
*            i_name  = 'Ocp-Apim-Subscription-Key'
*            i_value = '801c1e52d4c642428fcabfe4fd4661f3'        12/03/2025
*      RECEIVING
*        r_value =
*        ).               12/03/2025
*    CATCH cx_web_message_error.

*        req4->append_text( EXPORTING data = get_payload ).               12/03/2025
*        req4->set_content_type( 'application/json' ).                    12/03/2025
        DATA url_response2 TYPE string.

        TRY.
*            url_response2 = lv_client2->execute( if_web_http_client=>post )->get_text( ).           12/03/2025

*            TRANSLATE url_response2 TO UPPER CASE.

            TYPES: BEGIN OF ty_message,
                     ewbno        TYPE string,
                     ewbdt        TYPE string,
                     ewbvalidtill TYPE string,
                   END OF ty_message.

            TYPES: BEGIN OF ty_message2,
                     message TYPE ty_message,
                     status  TYPE string,
                   END OF ty_message2.



            TYPES: BEGIN OF ty_message3,
                     results TYPE ty_message2,
                   END OF ty_message3.

            DATA lv_message TYPE ty_message3.

*            xco_cp_json=>data->from_string( url_response2 )->write_to( REF #( lv_message ) ).

*            DATA(json_str) = url_response2.
*            DATA(json_result) = xco_cp_json=>read( url_response2 ).
            DATA: wa_zewaybillbyirn TYPE ztable_irn.
*
*            SELECT SINGLE * FROM ztable_irn AS a
*            WHERE a~billingdocno = @lv_invoice AND                              12/03/2025
*            a~bukrs = @lv_bukrs
*            INTO @DATA(lv_table_data).

*            wa_zewaybillbyirn = lv_table_data.
*            wa_zewaybillbyirn-ewaybillno = lv_message-results-message-ewbno.               12/03/2025
*            wa_zewaybillbyirn-ewaydate = lv_message-results-message-ewbdt.
*
*            MODIFY ztable_irn FROM @wa_zewaybillbyirn.
*
*            response->set_text( lv_message-results-message-ewbno ).
*

          CATCH cx_web_http_client_error INTO DATA(lv_error_response2).
            response->set_text( lv_error_response2->get_longtext( ) ).
        ENDTRY.
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
