CLASS zcl_http_irn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_service_extension .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_IRN IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    CASE request->get_method( ).
      WHEN CONV string( if_web_http_client=>post ).
        DATA token_url TYPE string .
        DATA lv_token TYPE string.
        DATA lv_client TYPE REF TO if_web_http_client.
        DATA req TYPE REF TO if_web_http_client.
        DATA irn_url TYPE string .
        DATA lv_client2 TYPE REF TO if_web_http_client.
        DATA req3 TYPE REF TO if_web_http_client.



        SELECT SINGLE FROM zr_integration_tab
        FIELDS Intgpath
        WHERE Intgmodule = 'IRN-CREATE-URL'
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
            ELSEIF lv_table_data1 IS NOT INITIAL.
              response->set_text( 'IRN is aready generated' ).
              RETURN.
            ENDIF.





            DATA(get_payload) = zcl_irn_generation=>generated_irn( companycode = lv_bukrs document = lv_invoice ).

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
                DATA(lv_response) = lv_client2->execute( if_web_http_client=>put ).

                url_response2 = lv_response->get_text( ).
                DATA(lv_status_code) = lv_response->get_status( )-code.
                IF lv_status_code <> 200.
                  response->set_status( i_code = lv_status_code i_reason = lv_response->get_status( )-reason ).
                  response->set_text( |API request failed with status { lv_status_code }: { lv_response->get_status( )-reason }| ).
                  RETURN.
                ENDIF.



                TYPES: BEGIN OF mainres,
                         deleted         TYPE string,
                         document_status TYPE string,
                       END OF mainres.

                DATA primeRes TYPE TABLE  OF mainres.


                xco_cp_json=>data->from_string( url_response2 )->write_to( REF #( primeRes ) ).

                LOOP AT primeRes INTO DATA(primeResChild).
                  IF primeResChild-document_status = 'NOT_CREATED'.

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


                  TYPES: BEGIN OF govt_res,
                           Success       TYPE string,
                           AckNo         TYPE string,
                           AckDt         TYPE string,
                           Irn           TYPE string,
                           SignedInvoice TYPE string,
                           SignedQRCode  TYPE string,
                           Status        TYPE string,
*                        need to also add for ewbi
                         END OF govt_res.

                  TYPES: BEGIN OF govt_res3,
                           govt_response TYPE  govt_res,
                         END OF govt_res3.

                  DATA successRes TYPE TABLE OF govt_res3.

                  xco_cp_json=>data->from_string( url_response2 )->write_to( REF #( successRes ) ).

                  LOOP AT successRes INTO DATA(successResChild).
                    IF successResChild-govt_response-success = 'Y'.
                      DATA: wa_zirn TYPE ztable_irn.
                      SELECT SINGLE * FROM ztable_irn AS a
                        WHERE a~billingdocno = @lv_invoice AND
                        a~bukrs = @lv_bukrs
                        INTO @DATA(lv_table_data).

                      wa_zirn = lv_table_data.
                      wa_zirn-irnno = successResChild-govt_response-irn.
                      wa_zirn-irnstatus = 'GEN'.
                      wa_zirn-ackno = successResChild-govt_response-ackno.
                      wa_zirn-ackdate = successResChild-govt_response-ackdt.
                      wa_zirn-signedinvoice = successResChild-govt_response-signedinvoice.
                      wa_zirn-signedqrcode = successResChild-govt_response-signedqrcode.
                      MODIFY ztable_irn FROM @wa_zirn.

                      response->set_text( | IRN Generated Successfully { successResChild-govt_response-irn } for Document - { lv_invoice }  | ).
                      RETURN.
                    ENDIF.
                  ENDLOOP.
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
ENDCLASS.
