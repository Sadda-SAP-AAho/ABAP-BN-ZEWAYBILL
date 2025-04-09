CLASS zcl_exp_tax_inv_dr DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA : access_token TYPE string .
    CLASS-DATA : xml_file TYPE string .
    TYPES :
      BEGIN OF struct,
        xdp_template TYPE string,
        xml_data     TYPE string,
        form_type    TYPE string,
        form_locale  TYPE string,
        tagged_pdf   TYPE string,
        embed_font   TYPE string,
      END OF struct."


    CLASS-METHODS :
      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check ,

      read_posts
        IMPORTING
                  bill_doc        TYPE string
*                  company_code     TYPE string
        RETURNING VALUE(result12) TYPE string
        RAISING   cx_static_check .
  PROTECTED SECTION.
  PRIVATE SECTION.
    CONSTANTS lc_ads_render TYPE string VALUE '/ads.restapi/v1/adsRender/pdf'.
    CONSTANTS  lv1_url    TYPE string VALUE 'https://adsrestapi-formsprocessing.cfapps.jp10.hana.ondemand.com/v1/adsRender/pdf?templateSource=storageName&TraceLevel=2'  .
    CONSTANTS  lv2_url    TYPE string VALUE 'https://dev-tcul4uw9.authentication.jp10.hana.ondemand.com/oauth/token'  .
    CONSTANTS lc_storage_name TYPE string VALUE 'templateSource=storageName'.
    CONSTANTS lc_template_name TYPE string VALUE 'zsd_dn_tax_inv/zsd_dn_tax_inv'.
*    CONSTANTS lc_template_name TYPE string VALUE 'zexport_tax_inv/zexport_tax_inv'.
ENDCLASS.



CLASS ZCL_EXP_TAX_INV_DR IMPLEMENTATION.


  METHOD create_client .
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).

  ENDMETHOD .


  METHOD read_posts .
    DATA : plant_add   TYPE string.
    DATA : p_add1  TYPE string.
    DATA : p_add2 TYPE string.
    DATA : p_city TYPE string.
    DATA : p_dist TYPE string.
    DATA : p_state TYPE string.
    DATA : p_pin TYPE string.
    DATA : p_country   TYPE string,
           plant_name  TYPE string,
           plant_gstin TYPE string.



    SELECT single
     a~billingdocument ,
      a~billingdocumentdate ,
      a~creationdate,
      a~creationtime,
      a~documentreferenceid,
       b~referencesddocument ,
       b~plant,
        d~deliverydocumentbysupplier,
     e~gstin_no ,
     e~state_code2 ,
     e~plant_name1 ,
     e~address1 ,
     e~address2 ,
     e~city ,
     e~district ,
     e~state_name ,
     e~pin ,
     e~country ,
     g~supplierfullname,
     i~documentdate,
    j~irnno ,
    j~ackno ,
    j~ackdate ,
    j~billingdocno  ,    "invoice no
    j~billingdate ,
    j~signedqrcode ,
    b~SALESORGANIZATION ,
    l~SALESORGANIZATIONNAME
*12.03    k~YY1_DODate_SDH,
*12.03    k~yy1_dono_sdh
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentitem AS b ON a~BillingDocument = b~BillingDocument
    LEFT JOIN i_purchaseorderhistoryapi01 AS c ON b~batch = c~batch AND c~goodsmovementtype = '101'
    LEFT JOIN i_inbounddelivery AS d ON c~deliverydocument = d~inbounddelivery
    LEFT JOIN ztable_plant AS e ON e~plant_code = b~plant
    LEFT JOIN i_billingdocumentpartner AS f ON a~BillingDocument = f~BillingDocument
    LEFT JOIN I_Supplier AS g ON f~Supplier = g~Supplier
    LEFT JOIN i_materialdocumentitem_2 AS h ON h~purchaseorder = c~purchaseorder AND h~goodsmovementtype = '101'
    LEFT JOIN I_MaterialDocumentHeader_2 AS i ON h~MaterialDocument = i~MaterialDocument
    LEFT JOIN ztable_irn AS j ON j~billingdocno = a~BillingDocument AND a~CompanyCode = j~bukrs
    LEFT JOIN i_salesdocument AS k ON k~salesdocument = b~salesdocument
    LEFT JOIN I_SalesOrganizationText as l on l~SalesOrganization = b~SalesOrganization
    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_header).




      p_add1 = wa_header-address1 && ',' .
      p_add2 = wa_header-address2 && ','.
      p_dist = wa_header-district && ','.
      p_city = wa_header-city && ','.
      p_state = wa_header-state_name .
      p_pin =  wa_header-pin .
      p_country =  '(' &&  wa_header-country && ')' .


      CONCATENATE p_add1  p_add2  p_dist p_city   p_state '-' p_pin  p_country INTO plant_add SEPARATED BY space.

      plant_name = wa_header-plant_name1.
      plant_gstin = wa_header-gstin_no.


      """""""""""""""""""""""""""""""""   BILL TO """""""""""""""""""""""""""""""""
      SELECT SINGLE
    d~streetname ,         " bill to add
    d~streetprefixname1 ,   " bill to add
    d~streetprefixname2 ,   " bill to add
    d~cityname ,   " bill to add
    d~region ,  "bill to add
    d~postalcode ,   " bill to add
    d~districtname ,   " bill to add
    d~country  ,
    d~housenumber ,
    c~customername,
    e~regionname,
    f~countryname,
    c~taxnumber3,
    d~STREETSUFFIXNAME1,
    d~STREETSUFFIXNAME2
   FROM I_BillingDocument AS a
   LEFT JOIN i_billingdocumentpartner AS b ON b~billingdocument = a~billingdocument
   LEFT JOIN i_customer AS c ON c~customer = b~Customer
   left JOIN i_address_2 AS d ON d~AddressID = c~AddressID
   LEFT JOIN i_regiontext AS e ON e~Region = c~Region AND e~Language = 'E' AND c~Country = e~Country
   LEFT JOIN i_countrytext AS f ON d~Country = f~Country
   WHERE b~partnerFunction = 'RE' AND  a~BillingDocument = @bill_doc
   INTO @DATA(wa_bill)
   PRIVILEGED ACCESS.




      """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""SHIP TO  Address
     SELECT SINGLE
     d~streetname ,
     d~streetprefixname1 ,
     d~streetprefixname2 ,
     d~cityname ,
     d~region ,
     d~postalcode ,
     d~districtname ,
     d~country  ,
     d~housenumber ,
     c~customername ,
     a~soldtoparty ,
     e~regionname
    FROM I_BillingDocumentitem AS a
    LEFT JOIN i_billingdocumentpartner AS b ON b~billingdocument = a~billingdocument
    LEFT JOIN i_customer AS c ON c~customer = b~Customer
    LEFT JOIN i_address_2 AS d ON d~AddressID = c~AddressID
    LEFT JOIN I_RegionText AS e on e~Region = d~Region and e~Country = d~Country
    WHERE b~partnerFunction = 'RE'
    and c~Language = 'E'
    and a~BillingDocument = @bill_doc
    INTO @DATA(wa_ship)
    PRIVILEGED ACCESS.


      DATA : wa_ad5 TYPE string.
      wa_ad5 = wa_bill-PostalCode.
      CONCATENATE wa_ad5 wa_bill-CityName  wa_bill-DistrictName INTO wa_ad5 SEPARATED BY space.

      DATA : wa_ad5_ship TYPE string.
      wa_ad5_ship = wa_ship-PostalCode.
      CONCATENATE wa_ad5_ship wa_ship-CityName  wa_ship-DistrictName INTO wa_ad5_ship SEPARATED BY space.





      """""""""""""""""""""""""""""""""""ITEM DETAILS"""""""""""""""""""""""""""""""""""

      SELECT
        a~billingdocument,
        a~billingdocumentitem,
        a~product,
        a~netamount,
        b~handlingunitreferencedocument,
        b~material,
        b~handlingunitexternalid,
        c~packagingmaterial,
        d~productdescription,
        e~materialbycustomer ,
        f~consumptiontaxctrlcode  ,   "HSN CODE
        a~billingdocumentitemtext ,   "mat
*12.03        e~yy1_packsize_sd_sdi  ,  "i_avgpkg
        a~billingquantity  ,  "Quantity
        a~billingquantityunit  ,  "UOM
*12.03        e~yy1_packsize_sd_sdiu  ,   " package_qtyunit
*12.03        e~yy1_noofpack_sd_sdi  ,   " avg_content
        g~conditionratevalue   ,  " i_per
        g~conditionamount ,
        g~conditionbasevalue,
        g~conditiontype


        FROM I_BillingDocumentItem AS a
        LEFT JOIN i_handlingunititem AS b ON a~referencesddocument = b~handlingunitreferencedocument
        LEFT JOIN i_handlingunitheader AS c ON b~handlingunitexternalid = c~handlingunitexternalid
        LEFT JOIN i_productdescription AS d ON d~product = a~product
        LEFT JOIN I_SalesDocumentItem AS e ON e~SalesDocument = a~SalesDocument AND e~salesdocumentitem = a~salesdocumentitem
        LEFT JOIN i_productplantbasic AS f ON a~Product = f~Product
        LEFT JOIN i_billingdocumentitemprcgelmnt AS g ON g~BillingDocument = a~BillingDocument AND g~BillingDocumentItem = a~BillingDocumentItem
        WHERE a~billingdocument = @bill_doc
        INTO TABLE  @DATA(it_item)
        PRIVILEGED ACCESS.


*      out->write( it_item ).
      SELECT SUM( conditionamount )
  FROM i_billingdocitemprcgelmntbasic
  WHERE billingdocument = @bill_doc
    AND conditiontype = 'ZFRT'
    INTO @DATA(freight).



    SORT it_item BY BillingDocumentItem.
    DELETE ADJACENT DUPLICATES FROM it_item COMPARING BillingDocument BillingDocumentItem.

    DATA : discount TYPE p DECIMALS 3.

*      out->write( it_item ).
*    out->write( wa_header ).

    data: temp_add type string.
    temp_add = wa_bill-POSTALCODE.
    CONCATENATE temp_add wa_bill-CityName wa_bill-DistrictName into temp_add.


    DATA(lv_xml) =
    |<Form>| &&
    |<BillingDocumentNode>| &&
    |<AckDate>{ wa_header-ackdate }</AckDate>| &&
    |<AckNumber>{ wa_header-ackno }</AckNumber>| &&
    |<BillingDate>{ wa_header-billingdate }</BillingDate>| &&
    |<DocumentReferenceID>{ wa_header-DocumentReferenceID }</DocumentReferenceID>| &&
    |<Irn>{ wa_header-irnno }</Irn>| &&
    |<YY1_PLANT_COM_ADD_BDH>{ plant_add }</YY1_PLANT_COM_ADD_BDH>| &&
    |<YY1_PLANT_COM_NAME_BDH>{ plant_name }</YY1_PLANT_COM_NAME_BDH>| &&
    |<YY1_PLANT_COM_GSTIN_NO_BDH>{ plant_gstin }</YY1_PLANT_COM_GSTIN_NO_BDH>| &&
    |<Supplier>| &&
    |<CompanyCode>{ wa_header-SalesOrganization }</CompanyCode>| &&
    |</Supplier>| &&
    |<Company>| &&
    |<CompanyName>{ wa_header-SalesOrganizationName }</CompanyName>| &&
    |</Company>| &&
*12.03    |<YY1_dodatebd_BDH>{ wa_header-YY1_DODate_SDH }</YY1_dodatebd_BDH>| &&
*12.03    |<YY1_dono_bd_BDH>{ wa_header-YY1_DONo_SDH }</YY1_dono_bd_BDH>| &&
*    |<Plant>{ wa_header-Plant }</Plant>| &&
*    |<RegionName>{ wa_header-state_name }</RegionName>| &&
    |<BillToParty>| &&
    |<AddressLine3Text>{ wa_bill-STREETNAME }</AddressLine3Text>| &&
    |<AddressLine4Text>{ wa_bill-STREETPREFIXNAME1 }</AddressLine4Text>| &&
    |<AddressLine5Text>{ wa_bill-STREETPREFIXNAME2 }</AddressLine5Text>| &&
    |<AddressLine6Text>{ wa_bill-STREETSUFFIXNAME1 }</AddressLine6Text>| &&
    |<AddressLine7Text>{ wa_bill-STREETSUFFIXNAME2 }</AddressLine7Text>| &&
    |<AddressLine8Text>{ temp_add }</AddressLine8Text>| &&
*    |<Region>{ wa_bill-Region }</Region>| &&
    |<FullName>{ wa_bill-CustomerName }</FullName>| &&   " done
*12.03    |<Partner>{ wa_header-YY1_DONo_SDH }</Partner>| &&
    |<RegionName>{ wa_bill-RegionName }</RegionName>| &&
    |</BillToParty>| &&
    |<Items>|.



    LOOP AT it_item INTO DATA(wa_item).

*      SELECT SINGLE
*     a~trade_name
*     FROM zmaterial_table AS a
*     WHERE a~mat = @wa_item-Product
*     INTO @DATA(wa_item3).
*
*      IF wa_item3 IS NOT INITIAL.
*        DATA(lv_item) =
*        |<BillingDocumentItemNode>| &&
*        |<YY1_fg_material_name_BDI>{ wa_item3 }</YY1_fg_material_name_BDI>|.
*        CONCATENATE lv_xml  lv_item INTO lv_xml.
*      ELSE.
*        " Fetch Product Name from `i_producttext`
*        SELECT SINGLE
*        a~productname
*        FROM i_producttext AS a
*        WHERE a~product = @wa_item-Product
*        INTO @DATA(wa_item4).
*
*        DATA(lv_item4) =
*        |<BillingDocumentItemNode>| &&
*        |<YY1_fg_material_name_BDI>{ wa_item4 }</YY1_fg_material_name_BDI>|.
*        CONCATENATE lv_xml lv_item4 INTO lv_xml.
*      ENDIF.
      SHIFT wa_item-Product LEFT DELETING LEADING '0'.
      DATA(lv_item) =
      |<BillingDocumentItemNode>|.
      CONCATENATE lv_xml lv_item INTO lv_xml.


      DATA(lv_item_xml) =

      |<BillingDocumentItemText>{ wa_item-Product }</BillingDocumentItemText>| &&
      |<IN_HSNOrSACCode>{ wa_item-consumptiontaxctrlcode }</IN_HSNOrSACCode>| &&
      |<NetPriceAmount></NetPriceAmount>| &&                       " pending
      |<Plant></Plant>| &&                                         " pending
      |<Quantity>{ wa_item-BillingQuantity }</Quantity>| &&
      |<QuantityUnit>{ wa_item-BillingQuantityUnit }</QuantityUnit>| &&
      |<YY1_bd_zdif_BDI></YY1_bd_zdif_BDI>| &&                      " pending
      |<YY1_fg_material_name_BDI></YY1_fg_material_name_BDI>| &&    " Pending
      |<ITEMCODE>{ wa_item-Product }</ITEMCODE>| &&
      |<ITEMDESC>{ wa_item-ProductDescription }</ITEMDESC>| &&
      |<NetAmount>{ wa_item-NetAmount }</NetAmount>| &&
      |<ItemPricingConditions>|.
      CONCATENATE lv_xml lv_item_xml INTO lv_xml.

      SELECT
        a~conditionType  ,  "hidden conditiontype
        a~conditionamount ,  "hidden conditionamount
        a~conditionratevalue  ,  "condition ratevalue
        a~conditionbasevalue   " condition base value
        FROM I_BillingDocItemPrcgElmntBasic AS a
         WHERE a~BillingDocument = @bill_doc AND a~BillingDocumentItem = @wa_item-BillingDocumentItem
        INTO TABLE @DATA(lt_item2)
        PRIVILEGED ACCESS.

      LOOP AT lt_item2 INTO DATA(wa_item2).
        DATA(lv_item2_xml) =
        |<ItemPricingConditionNode>| &&
        |<ConditionAmount>{ wa_item2-ConditionAmount }</ConditionAmount>| &&
        |<ConditionBaseValue>{ wa_item2-ConditionBaseValue }</ConditionBaseValue>| &&
        |<ConditionRateValue>{ wa_item2-ConditionRateValue }</ConditionRateValue>| &&
        |<ConditionType>{ wa_item2-ConditionType }</ConditionType>| &&
        |</ItemPricingConditionNode>|.
        CONCATENATE lv_xml lv_item2_xml INTO lv_xml.
        CLEAR wa_item2.
      ENDLOOP.
      DATA(lv_item3_xml) =
      |</ItemPricingConditions>| &&
      |</BillingDocumentItemNode>|.

      CONCATENATE lv_xml lv_item3_xml INTO lv_xml.
      CLEAR lv_item.
      CLEAR lv_item_xml.
      CLEAR lt_item2.
      CLEAR wa_item.
    ENDLOOP.

    DATA(lv_payment_term) =
      |<PaymentTerms>| &&
      |<PaymentTermsName></PaymentTermsName>| &&    " pending
      |</PaymentTerms>|.

    CONCATENATE lv_xml lv_payment_term INTO lv_xml.

    DATA(lv_shiptoparty) =
    |<ShipToParty>| &&
    |<AddressLine2Text>{ wa_ship-CustomerName }</AddressLine2Text>| &&
    |<AddressLine3Text>{ wa_ship-StreetPrefixName1 }</AddressLine3Text>| &&
    |<AddressLine4Text>{ wa_ship-StreetPrefixName2 }</AddressLine4Text>| &&
    |<AddressLine5Text>{ wa_ship-StreetName }</AddressLine5Text>| &&
    |<AddressLine6Text>{ wa_ad5_ship }</AddressLine6Text>| &&
    |<AddressLine7Text></AddressLine7Text>| &&
    |<AddressLine8Text></AddressLine8Text>| &&
    |<FullName>{ wa_bill-Region }</FullName>| &&
    |<RegionName>{ wa_ship-RegionName }</RegionName>| &&
    |</ShipToParty>|.

    CONCATENATE lv_xml lv_shiptoparty INTO lv_xml.

    DATA(lv_supplier) =
    |<Supplier>| &&
    |<RegionName></RegionName>| &&                " pending
    |</Supplier>|.
    CONCATENATE lv_xml lv_supplier INTO lv_xml.

    DATA(lv_taxation) =
    |<TaxationTerms>| &&
    |<IN_BillToPtyGSTIdnNmbr>{ wa_bill-taxnumber3 }</IN_BillToPtyGSTIdnNmbr>| &&       " pending   IN_BillToPtyGSTIdnNmbr
    |</TaxationTerms>|.
    CONCATENATE lv_xml lv_taxation INTO lv_xml.

    DATA(lv_footer) =
    |</Items>| &&
    |</BillingDocumentNode>| &&
    |</Form>|.

    CONCATENATE lv_xml lv_footer INTO lv_xml.

    CLEAR wa_ad5.
    CLEAR wa_ad5_ship.
    CLEAR wa_bill.
    CLEAR wa_ship.
    CLEAR wa_header.



    REPLACE ALL OCCURRENCES OF '&' IN lv_xml WITH 'and'.
    REPLACE ALL OCCURRENCES OF '<=' IN lv_xml WITH 'let'.
    REPLACE ALL OCCURRENCES OF '>=' IN lv_xml WITH 'get'.


    CALL METHOD zcl_ads_print=>getpdf(
      EXPORTING
        xmldata  = lv_xml
        template = lc_template_name
      RECEIVING
        result   = result12 ).

  ENDMETHOD.
ENDCLASS.
