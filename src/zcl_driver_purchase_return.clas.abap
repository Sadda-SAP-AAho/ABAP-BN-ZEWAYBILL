CLASS ZCL_DRIVER_PURCHASE_RETURN DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
*    INTERFACES if_oo_adt_classrun .
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
    CONSTANTS lc_template_name TYPE string VALUE 'zsd_del_rm_return/zsd_del_rm_return'."'zpo/zpo_v2'."
    CONSTANTS company_code TYPE string VALUE 'GT00'.
*    CONSTANTS lc_template_name TYPE 'HDFC_CHECK/HDFC_MULTI_FINAL_CHECK'.

ENDCLASS.



CLASS ZCL_DRIVER_PURCHASE_RETURN IMPLEMENTATION.


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


    SELECT SINGLE
     a~billingdocument ,
     a~billingdocumentdate ,
     a~creationdate,
     a~creationtime,
     b~referencesddocument ,
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
     i~documentdate ,
     j~irnno ,
     j~billingdate ,
     k~TaxNumber3
*12.03     a~yy1_remark_bdh
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentitem AS b ON a~BillingDocument = b~BillingDocument
    LEFT JOIN i_purchaseorderhistoryapi01 AS c ON b~batch = c~batch AND c~goodsmovementtype = '101'
    LEFT JOIN i_inbounddelivery AS d ON c~deliverydocument = d~inbounddelivery
    LEFT JOIN ztable_plant AS e ON e~plant_code = b~plant
    LEFT JOIN ztable_irn AS j ON j~billingdocno = a~BillingDocument AND a~CompanyCode = j~bukrs
    LEFT JOIN i_billingdocumentpartner AS f ON a~BillingDocument = f~BillingDocument
    LEFT JOIN i_customer AS k ON k~customer = f~Customer
    LEFT JOIN I_Supplier AS g ON f~Supplier = g~Supplier
    LEFT JOIN i_materialdocumentitem_2 AS h ON h~purchaseorder = c~purchaseorder AND h~goodsmovementtype = '101'
    LEFT JOIN I_MaterialDocumentHeader_2 AS i ON h~MaterialDocument = i~MaterialDocument
    WHERE a~BillingDocument = '0090000127'
    INTO @DATA(wa_header)
   PRIVILEGED ACCESS.



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


    """"""""""""""""""Bill To address"""""""""""""""""
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
    e~regionname
   FROM I_BillingDocument AS a
   LEFT JOIN i_billingdocumentpartner AS b ON b~billingdocument = a~billingdocument
   LEFT JOIN i_customer AS c ON c~customer = b~Customer
   LEFT JOIN i_address_2 AS d ON d~AddressID = c~AddressID
   LEFT JOIN i_regiontext AS e ON e~Region = d~Region
   WHERE b~partnerFunction = 'RE'
   INTO @DATA(wa_bill)
   PRIVILEGED ACCESS.



    """""""""""""""""""""""""""""""""Company Details"""""""""""""""""""""""""""""""""""""

    SELECT
    a~billingdocument ,
    a~billingquantity ,
    a~billingquantityunit ,
    a~product ,
*12.03    c~yy1_hscode_pdi ,
    d~purchaseorderdate
*12.03    c~yy1_packingmode_pdi ,
*12.03    c~yy1_quantityperpack_pdi
    FROM i_billingdocumentitem as a
    LEFT JOIN i_purchaseorderhistoryapi01 AS b ON a~batch = b~batch
    LEFT JOIN i_purchaseorderitemapi01 AS c ON b~purchaseorder = c~purchaseorder
    LEFT JOIN I_PurchaseOrderAPI01 AS d ON d~PurchaseOrder = c~PurchaseOrder
    WHERE a~billingdocument = @bill_doc
    INTO TABLE  @DATA(lt_item)
    PRIVILEGED ACCESS.


     DATA(lv_header) =
     |<Form>| &&
     |<BillingDocumentNode>| &&
     |<Irn>{ wa_header-irnno }</Irn>| &&
     |<BillingDate>{ wa_header-billingdate }</BillingDate>| &&
     |<BillingDocument>{ wa_header-BillingDocument }</BillingDocument>| &&
     |<ReferenceSDDocument>{ wa_header-ReferenceSDDocument }</ReferenceSDDocument>| &&
     |<YY1_Challan_BD_PO_RM_BDH>{ wa_header-deliverydocumentbysupplier }</YY1_Challan_BD_PO_RM_BDH>| &&
     |<YY1_PLANT_COM_ADD_BDH>{ plant_add }</YY1_PLANT_COM_ADD_BDH>| &&
     |<YY1_PLANT_COM_NAME_BDH>{ plant_name }</YY1_PLANT_COM_NAME_BDH>| &&
     |<YY1_PLANT_GSTIN_NO_BDH>{ plant_gstin }</YY1_PLANT_GSTIN_NO_BDH>| &&
*12.03     |<YY1_REMARK_BDH>{ wa_header-yy1_remark_bdh }</YY1_REMARK_BDH>| &&
     |<StateCodeAndName>{ wa_header-state_name }</StateCodeAndName>|.


******************************************************************************************TRANSPORTER

Select Single
a~billingdocument
*12.03  b~suppliername ,
*12.03  b~taxnumber3
from i_billingdocument as a
*12.03 left join I_Supplier as b on b~Supplier = a~YY1_TransportDetails_BDH
where a~BillingDocument = @bill_doc
INTO @Data(wa_header2)
PRIVILEGED ACCESS.

*12.03   Data(lv_header2) =
*12.03     |<YY1_TransportDetails_BDHT>{ wa_header2-SupplierName }</YY1_TransportDetails_BDHT>| &&
*12.03     |<YY1_TransportGST_bd_h_BDH>{ wa_header2-TaxNumber3 }</YY1_TransportGST_bd_h_BDH>| .
*12.03     CONCATENATE lv_header lv_header2 into lv_header.


*******************************************************************************************END TRANSPORTER

******************************************************************************************VEHICLE NUM
   Select single
   b~vehiclenum ,
   a~billingdocument
   from i_billingdocument as a
   LEFT JOIN ztable_irn AS b ON b~billingdocno = a~BillingDocument AND a~CompanyCode = b~bukrs
   where a~billingdocument = @bill_doc
   into @data(wa_header3)
   PRIVILEGED ACCESS.

IF wa_header3-vehiclenum is not initial .
   Data(lv_header3) =
      |<YY1_VehicleNo_BDH>{ wa_header3-vehiclenum }</YY1_VehicleNo_BDH>| .
      CONCATENATE lv_header lv_header3 into lv_header.

Else .

  Select single
*12.03  a~yy1_vehicleno_bdh ,
  a~billingdocument
  from i_billingdocument as a
  where a~BillingDocument = @bill_doc
  into @data(wa_header4)
  PRIVILEGED ACCESS.

*12.03   Data(lv_header4) =
*12.03      |<YY1_VehicleNo_BDH>{ wa_header4-YY1_VehicleNo_BDH }</YY1_VehicleNo_BDH>| .
*12.03      CONCATENATE lv_header lv_header4 into lv_header.

ENDIF.
*****************************************************************************************END VEHICLE NUM

*****************************************************************************************CHALLAN DATE


select single
e~documentdate ,
a~billingdocument
from I_billingdocument as a
LEFT join i_billingdocumentitem as b on a~billingdocument = b~billingdocument
LEFT join i_purchaseorderhistoryapi01 as c on b~batch = c~batch
LEFT join I_MATERIALDOCUMENTITEM_2 as d on d~purchaseorder = c~purchaseorder
LEFT join I_MaterialDocumentHeader_2 as e on d~materialdocument = e~materialdocument
WHERE b~billingdocument = @bill_doc
INTO @DATA(wa_header5)
PRIVILEGED ACCESS.


DATA(lv_date) = wa_header5-DocumentDate.

" Format as YYYY-MM-DD
DATA(lv_formatted_date) = lv_date(4) && '-' && lv_date+4(2) && '-' && lv_date+6(2).

" Remove unwanted spaces (if any)
CONDENSE lv_formatted_date.


DATA(lv_header5) =
    |<YY1_challanDate_bd_rm_BDH>{ lv_formatted_date }</YY1_challanDate_bd_rm_BDH>| .

CONCATENATE lv_header lv_header5 into lv_header.


*****************************************************************************************CHALLAN DATE END

*****************************************************************************************DATE and REMOVAL

DATA(lv2_date) = wa_header-CreationDate.

" Format as YYYY-MM-DD
DATA(lv2_formatted_date) = lv2_date(4) && '-' && lv2_date+4(2) && '-' && lv2_date+6(2).

" Remove unwanted spaces (if any)
CONDENSE lv2_formatted_date.
DATA(lv_header7) =
    |<YY1_removal_goods_date_BDH>{ lv2_formatted_date }</YY1_removal_goods_date_BDH>|.
CONCATENATE lv_header lv_header7 into lv_header.

*****************************************************************************************DATE and REMOVAL END

*****************************************************************************************CREATION TIME

DATA: lv_time TYPE string,
      lv_formatted_time TYPE string.

lv_time = wa_header-CreationTime.

" Insert colons at appropriate positions
lv_formatted_time = lv_time(2) && ':' && lv_time+2(2) && ':' && lv_time+4(2).

" Remove any unwanted spaces (just in case)
CONDENSE lv_formatted_time.

DATA(lv_header8) =
    |<YY1_removal_goods_time_BDH>{ lv_formatted_time }</YY1_removal_goods_time_BDH>| .
CONCATENATE lv_header lv_header8 into lv_header.


*****************************************************************************************CREATION TIME END




     Data(lv_header6) =
     |<BillToParty>| &&
     |<AddressLine1Text>{ wa_bill-CustomerName }</AddressLine1Text>| &&
     |<AddressLine2Text>{ wa_bill-StreetName }</AddressLine2Text>| &&
     |<AddressLine3Text>{ wa_bill-StreetPrefixName1 }</AddressLine3Text>| &&
     |<AddressLine4Text>{ wa_bill-StreetPrefixName2 }</AddressLine4Text>| &&
     |<AddressLine5Text>{ wa_bill-CityName }</AddressLine5Text>| &&
     |<AddressLine6Text>{ wa_bill-DistrictName }</AddressLine6Text>| &&
     |<AddressLine7Text>{ wa_bill-PostalCode }</AddressLine7Text>| &&
     |<AddressLine8Text>{ wa_bill-Country }</AddressLine8Text>| &&
     |<Region>{ wa_bill-Region }</Region>| &&
     |<RegionName>{ wa_bill-RegionName }</RegionName>| &&
     |</BillToParty>| &&
     |<Items>| .

     CONCATENATE lv_header lv_header6 into lv_header.

LOOP at lt_item into data(wa_item).
     DATA(lv_item) =
     |<BillingDocumentItemNode>| &&
     |<MaterialName></MaterialName>| &&
     |<Quantity>{ wa_item-BillingQuantity }</Quantity>| &&
     |<QuantityUnit>{ wa_item-BillingQuantityUnit }</QuantityUnit>| .
*12.03     |<YY1_HSCode_bd_po_BDI>{ wa_item-YY1_HSCode_PDI }</YY1_HSCode_bd_po_BDI>| &&
*12.03     |<YY1_PackingMode_BD_PO_BDI>{ wa_item-yy1_packingmode_pdi }</YY1_PackingMode_BD_PO_BDI>| &&
*12.03     |<YY1_quantityperpackbd_BDI>{ wa_item-yy1_quantityperpack_pdi }</YY1_quantityperpackbd_BDI>|.


********************************************************************************PO DATE



*12.03   DATA(lv3_date) = wa_item-PurchaseOrderDate.

" Format as YYYY-MM-DD
DATA(lv3_formatted_date) = lv2_date(4) && '-' && lv2_date+4(2) && '-' && lv2_date+6(2).

" Remove unwanted spaces (if any)
CONDENSE lv3_formatted_date.
DATA(lv_item8) =
  |<YY1_PODate_BD_RM_ret_BDI>{ lv3_formatted_date }</YY1_PODate_BD_RM_ret_BDI>|.
CONCATENATE lv_item lv_item8 into lv_item.

********************************************************************************PO DATE END


**********************************************************************RATE ITEM LEVEL

     Select
     c~conditionratevalue
     from i_billingdocumentitem as a
     LEFT JOIN i_purchaseorderhistoryapi01 AS b ON a~batch = b~batch
     LEFT JOIN i_purorditmpricingelementapi01 AS c ON b~purchaseorder = c~purchaseorder
     where a~BillingDocument = @bill_doc
     and c~ConditionType IN ('PMP0','PPR0')
     INTO Table @data(lt_item2)
     PRIVILEGED ACCESS.

     LOOP at lt_item2 into data(wa_item2) .
     DATA(lv_item2) =
        |<YY1_RatePMP0_BD_RM_Ret_BDI>{ wa_item2-ConditionRateValue }</YY1_RatePMP0_BD_RM_Ret_BDI>| .
     ENDLOOP.

     CONCATENATE lv_item lv_item2 into lv_item.

**********************************************************************TEXT ELEMENT TEXT
    SELECT
    a~billingdocument
*12.03    b~yy1_no_of_pack_bdi
    FROM I_BillingDocumentitem As a
    LEFT JOIN I_BILLINGDOCUMENTITEMTP As b on b~BillingDocument = a~BillingDocument
    WHERE a~BillingDocument = @bill_doc
    INTO Table @Data(lt_item5)
    PRIVILEGED ACCESS.

     LOOP at lt_item5 into data(wa_item5) .
*12.03     DATA(lv_item5) =
*12.03        |<YY1_NO_OF_PACK_BDI>{ wa_item5-yy1_no_of_pack_bdi }</YY1_NO_OF_PACK_BDI>| .
     ENDLOOP.

*12.03     CONCATENATE lv_item lv_item5 into lv_item.


**********************************************************************TEXT ELEMNT TEXT END


***************************************************************************************TRADENAME BEGIN
      SELECT SINGLE
      a~trade_name
      FROM zmaterial_table AS a
      WHERE a~mat = @wa_item-Product
      INTO  @DATA(wa_item6).

      IF wa_item6 IS NOT INITIAL.
        DATA(lv_item6) =
        |<YY1_fg_material_name_BDI>{ wa_item6 }</YY1_fg_material_name_BDI>|.
        CONCATENATE lv_item lv_item6 INTO lv_item .
      ELSE.
        " Fetch Product Name from `i_producttext`
        SELECT SINGLE
        a~productname
        FROM i_producttext AS a
        WHERE a~product = @wa_item-Product
        INTO @DATA(wa_item7).

        DATA(lv_item7) =
        |<YY1_fg_material_name_BDI>{ wa_item7 }</YY1_fg_material_name_BDI>|.
        CONCATENATE lv_item lv_item7 INTO lv_item.
      ENDIF.
***************************************************************************************TRADENAME END



**********************************************************************TAXCODE NAME

    SELECT
    d~taxcodename
    FROM I_BillingDocumentItem as a
    LEFT JOIN i_purchaseorderhistoryapi01 AS b ON b~batch = a~batch
    INNER JOIN i_purorditmpricingelementapi01 AS c ON b~purchaseorder = c~purchaseorder AND b~purchaseorderitem = c~purchaseorderitem
    INNER JOIN i_taxcodetext AS d ON c~taxcode = d~taxcode
    WHERE c~ConditionType = 'ZJEX'
    INTO TABLE @DATA(lt_item3)
    PRIVILEGED ACCESS.

    LOOP at lt_item3 into data(wa_item3) .
     DATA(lv_item3) =
        |<YY1_TaxCodeName_bd_rm_BDI>{ wa_item3-TaxCodeName }</YY1_TaxCodeName_bd_rm_BDI>| .
     ENDLOOP.

     CONCATENATE lv_item lv_item3 into lv_item.

     CONCATENATE lv_item '<ItemPricingConditions>' INTO lv_item.

**********************************************************************TAXCODE NAME END

SELECT
     a~conditionType  ,  "hidden conditiontype
     a~conditionamount ,  "hidden conditionamount
     a~conditionratevalue   "condition ratevalue
     FROM I_BillingDocItemPrcgElmntBasic AS a
     WHERE a~BillingDocument = @bill_doc
     INTO TABLE @DATA(lt_item4)
     PRIVILEGED ACCESS.


     LOOP AT lt_item4 INTO DATA(wa_item4) .
        DATA(lv_item4) =
        |<ItemPricingConditionNode>| &&
        |<ConditionAmount>{ wa_item4-ConditionAmount }</ConditionAmount>| &&
        |<ConditionRateValue>{ wa_item4-ConditionRateValue }</ConditionRateValue>| &&
        |<ConditionType>{ wa_item4-ConditionType }</ConditionType>| &&
        |</ItemPricingConditionNode>|.

     CONCATENATE lv_item lv_item4 INTO lv_item.

      ENDLOOP.

      DATA(it3) = |</ItemPricingConditions>| .
        CONCATENATE lv_item it3 INTO lv_item.

      CLEAR wa_item.
      CLEAR wa_item2.
      CLEAR wa_item3.
      CLEAR wa_item4.
      CLEAR wa_item5.
ENDLOOP.

CONCATENATE lv_header lv_item into lv_header.

     DATA(lv_footerend) =
     |</BillingDocumentItemNode>| .

     CONCATENATE lv_header lv_footerend into lv_header.

     DATA(lv_footer) =
     |</Items>| &&
     |<Supplier>| &&
     |<Region>{ wa_bill-Region }</Region>| &&
     |<RegionName>{ wa_header-state_name }</RegionName>| &&
     |</Supplier>| &&
     |<TaxationTerms>| &&
     |<TaxNumber3>{ wa_header-TaxNumber3 }</TaxNumber3>| &&
     |</TaxationTerms>| &&
     |</BillingDocumentNode>| &&
     |</Form>|.

     CONCATENATE lv_header lv_footer into lv_header.

    REPLACE ALL OCCURRENCES OF '&' IN lv_header WITH 'and'.
    REPLACE ALL OCCURRENCES OF '<=' IN lv_header WITH 'let'.
    REPLACE ALL OCCURRENCES OF '>=' IN lv_header WITH 'get'.

  CALL METHOD zcl_ads_print=>getpdf(
      EXPORTING
        xmldata  = lv_header
        template = lc_template_name
      RECEIVING
        result   = result12 ).



  ENDMETHOD.
ENDCLASS.
