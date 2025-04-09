CLASS lhc_ZIRN DEFINITION INHERITING FROM cl_abap_behavior_handler.
  PRIVATE SECTION.

    METHODS get_global_authorizations FOR GLOBAL AUTHORIZATION
      IMPORTING REQUEST requested_authorizations FOR zirn RESULT result.

    METHODS createIRNData FOR MODIFY
      IMPORTING keys FOR ACTION zirn~createIRNData RESULT result.
    METHODS Irn FOR MODIFY
      IMPORTING keys FOR ACTION zirn~Irn RESULT result.
    METHODS PrintForm FOR MODIFY
      IMPORTING keys FOR ACTION zirn~PrintForm RESULT result.

ENDCLASS.

CLASS lhc_ZIRN IMPLEMENTATION.

  METHOD get_global_authorizations.
  ENDMETHOD.

  METHOD createIRNData.
    DATA : lt_irn TYPE TABLE OF ztable_irn.
    DATA : wa_irn TYPE ztable_irn.
    READ TABLE keys INTO DATA(ls_key) INDEX 1.
    DATA(plant) = ls_key-%param-PlantNo.
    DATA(plandate) = ls_key-%param-PlanDate.

    SELECT FROM i_billingdocumentitem AS a
    LEFT JOIN i_customer AS b ON a~PayerParty = b~Customer
        FIELDS
        a~CompanyCode,
        a~BillingDocument,
        a~BillingDocumentDate,
        a~Plant,
        a~PayerParty,
        b~CustomerName
        WHERE a~Plant = @plant
        AND a~BillingDocumentDate = @plandate AND
        a~BillingDocument NOT IN ( SELECT billingdocno FROM ztable_irn WHERE billingdocno IS NOT INITIAL )
        INTO TABLE @DATA(lt).

    SORT lt BY BillingDocument.
    DELETE ADJACENT DUPLICATES FROM lt COMPARING BillingDocument CompanyCode.
    GET TIME STAMP FIELD DATA(lv_timestamp).
    LOOP AT lt INTO DATA(wa).

      MODIFY ENTITIES OF zr_zirntp IN LOCAL MODE
    ENTITY zirn
    CREATE
    FIELDS ( Bukrs billingdocno billingdate plant partycode partyname moduletype lastchangedat )
    WITH VALUE #(
      ( %cid = 'cidap'
      Bukrs = wa-CompanyCode
        billingdocno = wa-BillingDocument
        billingdate = wa-BillingDocumentDate
        plant = wa-Plant
        Partycode = wa-PayerParty
        Partyname = wa-CustomerName
        Moduletype = 'SALES'
        lastchangedat = lv_timestamp )
    )
    MAPPED mapped
    FAILED   failed
    REPORTED reported.
      .

      CLEAR wa.
    ENDLOOP.
    APPEND VALUE #( %cid = ls_key-%cid
                    %msg = new_message_with_text(
                      severity = if_abap_behv_message=>severity-success
                      text = 'Data Generated.' )
                      ) TO reported-zirn.
    RETURN.
  ENDMETHOD.

  METHOD Irn.

    DATA token_url TYPE string .
    DATA lv_token TYPE string.
    DATA lv_client TYPE REF TO if_web_http_client.
    DATA req TYPE REF TO if_web_http_client.
    DATA irn_url TYPE string .
    DATA lv_client2 TYPE REF TO if_web_http_client.
    DATA req3 TYPE REF TO if_web_http_client.

    token_url = 'https://sandb-api.mastersindia.co/api/v1/token-auth/' .
    irn_url = 'https://sandb-api.mastersindia.co/api/v1/einvoice/' .
    TRY.
        DATA(dest) = cl_http_destination_provider=>create_by_url( token_url ).
        lv_client = cl_web_http_client_manager=>create_by_http_destination( dest ).

      CATCH cx_static_check INTO DATA(lv_cx_static_check).
*        response->set_text( lv_cx_static_check->get_longtext( ) ).
        CLEAR dest.
    ENDTRY.

    TYPES: BEGIN OF ty_body,
             username TYPE string,
             password TYPE string,
           END OF ty_body.

    DATA ls_body TYPE ty_body.
    ls_body-username = 'aman@mastersindia.co'.
*    ls_body-password = 'Miitspl@123'.          "comment by SK

    DATA(lv_json) = /ui2/cl_json=>serialize( data = ls_body compress = abap_true pretty_name = /ui2/cl_json=>pretty_mode-low_case ).
    DATA(req2) = lv_client->get_http_request(  ).
    req2->append_text(
                EXPORTING
                  data   = lv_json
              ).
    req2->set_content_type( 'application/json' ).
    DATA url_response TYPE string.

    TRY.
        url_response = lv_client->execute( if_web_http_client=>post )->get_text( ).
        REPLACE ALL OCCURRENCES OF '{"token":"' IN url_response WITH ''.
        REPLACE ALL OCCURRENCES OF '"}' IN url_response WITH ''.
        lv_token = url_response.
*        response->set_text( url_response ).
      CATCH cx_web_http_client_error INTO DATA(lv_error_response).
*        response->set_text( lv_error_response->get_longtext( ) ).
        CLEAR lv_token.
    ENDTRY.



    TRY.
        DATA(dest2) = cl_http_destination_provider=>create_by_url( irn_url ).
        lv_client2 = cl_web_http_client_manager=>create_by_http_destination( dest2 ).

      CATCH cx_static_check INTO DATA(lv_cx_static_check2).
        CLEAR dest2.
*        response->set_text( lv_cx_static_check2->get_longtext( ) ).
    ENDTRY.

    READ TABLE keys INTO DATA(ls_key) INDEX 1.

    DATA(lv_payload) = zcl_irn_generation=>generated_irn( companycode = ls_key-Bukrs  document = ls_key-Billingdocno ).

    DATA(req4) = lv_client2->get_http_request( ).
*
    lv_token = |JWT { lv_token }|.
    req4->set_header_field(
      EXPORTING
        i_name  = 'Authorization'
        i_value = lv_token
*      RECEIVING
*        r_value =
    ).
*    CATCH cx_web_message_error.

    req4->append_text( EXPORTING data = lv_payload ).
    req4->set_content_type( 'application/json' ).
    DATA url_response2 TYPE string.

    TRY.
        url_response2 = lv_client2->execute( if_web_http_client=>post )->get_text( ).


*        response->set_text( url_response2 ).

      CATCH cx_web_http_client_error INTO DATA(lv_error_response2).
*        response->set_text( lv_error_response->get_longtext( ) ).
        CLEAR url_response2.
    ENDTRY.

  ENDMETHOD.

  METHOD PrintForm.

  ENDMETHOD.

ENDCLASS.
