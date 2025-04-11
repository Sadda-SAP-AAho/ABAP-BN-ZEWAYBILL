CLASS zcl_http_generateirn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_http_service_extension.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_HTTP_GENERATEIRN IMPLEMENTATION.


  METHOD if_http_service_extension~handle_request.
    DATA(req) = request->get_form_fields(  ).
    DATA(body)  = request->get_text(  )  .
*    xco_cp_json=>data->from_string( body )->write_to( REF #( lv_respo ) ).
*    /ui2/cl_json=>deserialize(
*    EXPORTING
*        json = body
*    CHANGING
*        data = lv_respo
*    ).

    response->set_header_field( i_name = 'Access-Control-Allow-Origin' i_value = '*' ).
    response->set_header_field( i_name = 'Access-Control-Allow-Credentials' i_value = 'true' ).
    DATA(cookies)  = request->get_cookies(  ) .

    DATA req_host TYPE string.
    DATA req_proto TYPE string.
    DATA req_uri TYPE string.
    DATA json TYPE string .

    req_host = request->get_header_field( i_name = 'Host' ).
    req_proto = request->get_header_field( i_name = 'X-Forwarded-Proto' ).
    CASE request->get_method( ).

      WHEN CONV string( if_web_http_client=>get ).

*        response->set_text( get_html( ) ).

      WHEN CONV string( if_web_http_client=>post ).

*        DATA(plant) = lv_respo-plant.
*        DATA(docdate) = lv_respo-docdate.
        DATA: plant   TYPE ztable_irn-plant.
       DATA: docdate TYPE d.
        plant = to_upper( request->get_form_field( `plant` ) ).
       docdate = request->get_form_field( `docdate` ).
        SELECT FROM i_billingdocumentitem AS a
         inner join I_billingdocument as c on a~BillingDocument = c~BillingDocument
        LEFT JOIN i_customer AS b ON a~PayerParty = b~Customer
        FIELDS
        a~CompanyCode,
        a~BillingDocument,
        a~CreationDate,
        a~Plant,
        a~PayerParty,
        b~CustomerName,
        a~DistributionChannel,
        a~BillingDocumentType, c~DocumentReferenceID,a~ReferenceSDDocument
        WHERE a~Plant = @plant
        AND a~CreationDate = @docdate
         AND
        a~BillingDocument NOT IN ( SELECT billingdocno FROM ztable_irn WHERE billingdocno IS NOT INITIAL )
        INTO TABLE @DATA(lt).

        SORT lt BY BillingDocument.
        DELETE ADJACENT DUPLICATES FROM lt COMPARING BillingDocument CompanyCode.

        select single from ztable_plant as a
        fields a~gstin_no
        where a~plant_code = @plant
        into @data(gst).

        DATA: wa_zirn TYPE ztable_irn.
        GET TIME STAMP FIELD DATA(lv_timestamp).
        LOOP AT lt INTO DATA(wa).

          SHIFT wa-ReferenceSDDocument LEFT DELETING LEADING '0'.

            SELECT SINGLE
            d~vehicleno ,
            d~LRNo ,
            d~transportmode ,
            d~transportername ,
            d~lrdate
            FROM zr_gateentrylines AS c
            JOIN ZR_GateEntryHeader AS d ON d~Gateentryno = c~Gateentryno
            WHERE c~Documentno = @wa-ReferenceSDDocument
            INTO @DATA(wa_gatemain).

          wa_zirn-grno = wa_gatemain-LRNo.
          wa_zirn-grdate = wa_gatemain-lrdate.
          wa_zirn-transportername = wa_gatemain-Transportername.
          wa_zirn-vehiclenum = wa_gatemain-Vehicleno.
          wa_zirn-transportmode = wa_gatemain-Transportmode.

          wa_zirn-Bukrs = wa-CompanyCode.
          wa_zirn-billingdocno = wa-BillingDocument.
          wa_zirn-billingdate = wa-CreationDate.
          wa_zirn-documentreferenceid = wa-DocumentReferenceID.
          wa_zirn-plant = wa-Plant.
          wa_zirn-gstno = gst.
          wa_zirn-Partycode = wa-PayerParty.
          wa_zirn-distributionchannel = wa-DistributionChannel.
          wa_zirn-billingdocumenttype = wa-BillingDocumentType.
          wa_zirn-Partyname = wa-CustomerName.
          wa_zirn-Moduletype = 'SALES'.
          wa_zirn-last_changed_at = lv_timestamp.
          MODIFY ztable_irn FROM @wa_zirn.
          CLEAR wa_zirn.
          CLEAR wa.
        ENDLOOP.
        response->set_text( '1' ).
    ENDCASE.
  ENDMETHOD.
ENDCLASS.
