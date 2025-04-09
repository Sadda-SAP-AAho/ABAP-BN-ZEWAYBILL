CLASS zcl_http_cancelirn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_http_service_extension .

    CLASS-METHODS :getPayload IMPORTING
                                        invoice       TYPE ztable_irn-billingdocno
                                        companycode   TYPE ztable_irn-bukrs
                              RETURNING VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_http_cancelirn IMPLEMENTATION.

  METHOD if_http_service_extension~handle_request.
    CASE request->get_method(  ).
      WHEN CONV string( if_web_http_client=>post ).
        DATA irn_url TYPE string.
        DATA lv_client2 TYPE REF TO if_web_http_client.
        SELECT SINGLE FROM zr_integration_tab
        FIELDS Intgpath
        WHERE Intgmodule = 'IRN-CANCEL-URL'
        INTO @irn_url.


        TRY.
            DATA(dest2) = cl_http_destination_provider=>create_by_url( irn_url ).
            lv_client2 = cl_web_http_client_manager=>create_by_http_destination( dest2 ).

          CATCH cx_static_check INTO DATA(lv_cx_static_check2).
            response->set_status( i_code = 500 i_reason = 'Internal Server Error' ).
            response->set_text( |Destination creation failed: { lv_cx_static_check2->get_longtext( ) }| ).
            RETURN.
        ENDTRY.

        DATA: lv_bukrs TYPE ztable_irn-bukrs.
        DATA: lv_invoice TYPE ztable_irn-billingdocno.
        DATA: lv_gstno TYPE string.

        TRY.
            lv_bukrs = request->get_form_field( `companycode` ).
            lv_invoice = request->get_form_field( `document` ).

            SELECT SINGLE FROM ztable_irn AS a
            FIELDS a~irnno
               WHERE a~billingdocno = @lv_invoice AND
               a~bukrs = @lv_bukrs
               INTO @DATA(lv_table_data1).


            IF lv_bukrs IS INITIAL OR lv_invoice IS INITIAL.
              response->set_text( 'Company code and document number are required' ).
              RETURN.
            ELSEIF lv_table_data1 IS INITIAL.
              response->set_text( 'IRN Not Generated' ).
              RETURN.
            ENDIF.


            DATA(get_payload) = getpayload( companycode = lv_bukrs invoice = lv_invoice ).

            SELECT SINGLE FROM I_BillingDocumentItem AS b
                  FIELDS b~Plant, b~BillingDocumentType
                  WHERE b~BillingDocument = @lv_invoice
                  INTO @DATA(lv_document_details) PRIVILEGED ACCESS.

            IF sy-subrc <> 0.
              response->set_text( |Document { lv_invoice } not found| ).
              RETURN.
            ENDIF.

            SELECT SINGLE FROM ztable_plant
                FIELDS gstin_no
                WHERE comp_code = @lv_bukrs AND plant_code = @lv_document_details-Plant
                INTO @DATA(userPass).

            IF userPass IS INITIAL.
              response->set_status( i_code = 404 i_reason = 'Not Found' ).
              response->set_text( |GSTIN not found for company { lv_bukrs } and plant { lv_document_details-Plant }| ).
              RETURN.
            ENDIF.

            DATA(req4) = lv_client2->get_http_request( ).

            SELECT SINGLE FROM zr_integration_tab
             FIELDS Intgpath
             WHERE Intgmodule = 'IRN-HEAD'
             INTO @DATA(subscription).

            SPLIT subscription  AT ':' INTO DATA(head1name) DATA(head1val).

            req4->set_header_field(
               i_name  = head1name
               i_value = head1val
             ).
            req4->set_header_field(
               i_name  = 'gstin'
               i_value = CONV string( userPass )
             ).

            req4->append_text( EXPORTING data = get_payload ).
            req4->set_content_type( 'application/json' ).
            DATA url_response2 TYPE string.


            TRY.
                url_response2 = lv_client2->execute( if_web_http_client=>put )->get_text( ).

                TYPES: BEGIN OF mainres,
                         deleted         TYPE string,
                         document_status TYPE string,
                       END OF mainres.

                DATA primeRes TYPE TABLE  OF mainres.


                xco_cp_json=>data->from_string( url_response2 )->write_to( REF #( primeRes ) ).

                LOOP AT primeRes INTO DATA(primeResChild).
                  IF primeResChild-document_status = 'IRN_CANCELLATION_FAILED'.

                    TYPES: BEGIN OF errorDetails,
                             error_code    TYPE string,
                             error_message TYPE string,
                             error_source  TYPE string,
                           END OF errorDetails.

                    TYPES: BEGIN OF govt_res1,
                             Success      TYPE string,
                             ErrorDetails TYPE TABLE OF errorDetails WITH EMPTY KEY,
                           END OF govt_res1.

                    TYPES: BEGIN OF govt_res2,
                             govt_response TYPE  govt_res1,
                           END OF govt_res2.

                    DATA errorRes TYPE TABLE OF govt_res2.

                    xco_cp_json=>data->from_string( url_response2 )->write_to( REF #( errorRes ) ).

                    LOOP AT errorRes INTO DATA(errorResChild).
                      IF errorResChild-govt_response-success = 'N'.
                        LOOP AT errorResChild-govt_response-errordetails INTO DATA(error).
                          response->set_text( error-error_message ).
                          RETURN.
                        ENDLOOP.

                      ELSE.
                        response->set_text( 'Something went wrong' ).
                        RETURN.
                      ENDIF.
                    ENDLOOP.

                  ENDIF.

                  DATA: wa_zirn TYPE ztable_irn.
                  SELECT SINGLE * FROM ztable_irn AS a
                    WHERE a~billingdocno = @lv_invoice AND
                    a~bukrs = @lv_bukrs
                    INTO @DATA(lv_table_data).

                  wa_zirn = lv_table_data.
                  wa_zirn-irnno = ''.
                  wa_zirn-ackno = ''.
                  wa_zirn-ackdate = ''.
                  wa_zirn-signedinvoice = ''.
                  wa_zirn-signedqrcode = ''.
                  wa_zirn-irnstatus = 'CNL'.
                  wa_zirn-irncanceldate = sy-datum.
                  MODIFY ztable_irn FROM @wa_zirn.


                  response->set_text( |IRN Cancelled| ).
                ENDLOOP.
              CATCH cx_web_http_client_error INTO DATA(lv_error_response2).
                response->set_status( i_code = 500 i_reason = 'Internal Server Error' ).
                response->set_text( |API request failed: { lv_error_response2->get_longtext( ) }| ).
            ENDTRY.
          CATCH cx_root INTO DATA(lv_general_error).
            response->set_status( i_code = 500 i_reason = 'Internal Server Error' ).
            response->set_text( |Processing failed: { lv_general_error->get_longtext( ) }| ).
        ENDTRY.
      WHEN OTHERS.
        response->set_status( i_code = 405 i_reason = 'Method Not Allowed' ).
        response->set_text( 'Only POST method is supported' ).
    ENDCASE.
  ENDMETHOD.


  METHOD getPayload.


    TYPES: BEGIN OF ty_item_list,
             irn    TYPE string,
             CnlRsn TYPE string,
             CnlRem TYPE string,
           END OF ty_item_list.

    DATA : wa_json TYPE ty_item_list.

    SELECT SINGLE FROM ztable_irn AS a
    FIELDS a~irnno
     WHERE a~billingdocno = @invoice AND
     a~bukrs = @companycode
     INTO @DATA(lv_table_data).

    wa_json-irn = lv_table_data.
    wa_json-cnlrem = 'Wrong'.
    wa_json-cnlrsn = '1'.


    DATA:json TYPE REF TO if_xco_cp_json_data.

    xco_cp_json=>data->from_abap(
      EXPORTING
        ia_abap      = wa_json
      RECEIVING
        ro_json_data = json   ).
    json->to_string(
      RECEIVING
        rv_string =   DATA(lv_string) ).

    REPLACE ALL OCCURRENCES OF '"IRN"' IN lv_string WITH '"Irn"'.
    REPLACE ALL OCCURRENCES OF '"CNLRSN"' IN lv_string WITH '"CnlRsn"'.
    REPLACE ALL OCCURRENCES OF '"CNLREM"' IN lv_string WITH '"CnlRem"'.

    result = |[{ lv_string }]|.

  ENDMETHOD.
ENDCLASS.
