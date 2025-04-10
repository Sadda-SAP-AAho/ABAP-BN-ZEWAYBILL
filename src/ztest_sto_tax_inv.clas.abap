CLASS ztest_sto_tax_inv DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    CLASS-DATA : bill_doc TYPE I_BillingDocument-BillingDocument.
    CLASS-DATA : company_code TYPE I_BillingDocument-CompanyCode.
    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ztest_sto_tax_inv IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

bill_doc = '0090000635'.
company_code = 'GT00'.

    DATA : plant_add   TYPE string.
    DATA : p_add1  TYPE string.
    DATA : p_add2 TYPE string.
    DATA : p_city TYPE string.
    DATA : p_dist TYPE string.
    DATA : p_state TYPE string.
    DATA : p_country   TYPE string,
           plant_name  TYPE string,
           plant_gstin TYPE string,
           p_pin       TYPE string.



    SELECT SINGLE
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
    b~salesorganization ,
    l~salesorganizationname ,
    j~ewaybillno ,
    j~ewaydate ,
    e~fssai_no ,
    a~incotermsclassification ,
    a~incotermslocation1 ,
    m~telephonenumber1 ,
    a~PurchaseOrderByCustomer
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
    LEFT JOIN I_SalesOrganizationText AS l ON l~SalesOrganization = b~SalesOrganization
    LEFT JOIN I_Customer AS m ON m~Customer = a~payerparty

    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_header).


**********************************************************************CURRENT DATE

DATA(lv_currdate) = cl_abap_context_info=>get_system_date( ).
DATA: lv_datecurr      TYPE string.

    lv_datecurr = lv_currdate .
    CONDENSE lv_datecurr NO-GAPS.  " Remove any spaces
    lv_datecurr = lv_datecurr+6(2) && '/' && lv_datecurr+4(2) && '/' && lv_datecurr(4).


**********************************************************************CURRENT DATE END




**********************************************************************BILLTOPARTNER

    SELECT SINGLE
    a~billingdocument ,
    a~payerparty
    FROM i_billingdocument AS a
    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_bpart).

    SHIFT wa_bpart-PayerParty LEFT DELETING LEADING '0'.


***********************************************************************************

**********************************************************************SHIPTOPARTNER

    SELECT SINGLE
    a~billingdocument ,
    b~shiptoparty
    FROM I_BillingDocumentItem AS a
    LEFT JOIN I_DeliveryDocument AS b ON b~DeliveryDocument = a~ReferenceSDDocument
    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_spart) .


    SHIFT wa_spart-ShipToParty LEFT DELETING LEADING '0'.

***********************************************************************************

**********************************************************************BANKDETAILS
    SELECT SINGLE FROM i_billingdocumentitem AS a
    LEFT JOIN ztable_irn AS b ON b~billingdocno = a~BillingDocument
    LEFT JOIN zbank_tab AS c ON c~salesorg = b~bukrs AND  c~distributionchannel = b~distributionchannel
    FIELDS c~acoount_number , c~bank_details , c~ifsc_code , c~distributionchannel ,c~salesorg
    WHERE a~billingdocument = @bill_doc  " AND c~distributionchannel IN ( 'BS','HS','TS','OS' )
    INTO @DATA(wa_bank).

    DATA : distch TYPE string.
    DATA : salesor TYPE string.
    distch = wa_bank-distributionchannel .
    salesor = wa_bank-salesorg .
**********************************************************************BANKDETAILS END


**********************************************************************SHIPTOPHONE

    SELECT SINGLE
    c~telephonenumber1,
    a~billingdocument
    FROM i_BillingDocumentitem AS a
    LEFT JOIN i_deliverydocument AS b ON b~deliverydocument = a~referencesddocument
    LEFT JOIN i_customer AS c ON c~Customer = b~shiptoparty
    WHERE a~billingdocument = @bill_doc
    INTO @DATA(wa_sp) PRIVILEGED ACCESS.


************************************************************************************
*    SELECT single from zgateentryheader as a
*        left join zgateentrylines as b on a~gateentryno = b~gateentryno
*      fields a~gateentryno, b~documentno
*        where a~gateentryno = b~gateentryno
*        into @data(wa_gateno).

**********************************************************************LR NO & VEHICLENO & TRANSPORTMODE & LR DATE

    SELECT SINGLE
    a~billingdocument ,
    b~referencesddocument
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentitem AS b ON b~billingdocument = a~billingdocument
    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_lrvntm).

    SHIFT wa_lrvntm-ReferenceSDDocument LEFT DELETING LEADING '0'.

    SELECT SINGLE
    d~vehicleno ,
    d~LRNo ,
    d~transportmode ,
    d~transportername ,
    d~lrdate
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentitem AS b ON b~billingdocument = a~billingdocument
    LEFT JOIN zr_gateentrylines AS c ON c~Documentno = @wa_lrvntm-ReferenceSDDocument
    LEFT JOIN ZR_GateEntryHeader AS d ON d~Gateentryno = c~Gateentryno
    WHERE a~billingdocument = @bill_doc
    INTO @DATA(wa_gatemain).

***********************************************************************LR NO & VEHICLENO & TRANSPORTMODE & LR DATE END

**********************************************************************ZIRN LOGIC FOR LR NO & VEHICLENO & TRANSPORTMODE & LR DATE END

SELECT SINGLE FROM I_BillingDocument AS A
LEFT JOIN ztable_irn AS B ON B~billingdocno = A~BillingDocument
FIELDS A~BillingDocument , B~vehiclenum , B~transportername , B~grno , B~grdate
WHERE A~BillingDocument = @bill_doc
INTO @DATA(WA_IRN).


**********************************************************************ZIRN LOGIC FOR LR NO & VEHICLENO & TRANSPORTMODE & LR DATE END

    p_add1 = wa_header-address1 .
    p_add2 = wa_header-address2 .
    p_dist = wa_header-district .
    p_city = wa_header-city .
    p_state = wa_header-state_name .
    p_pin =  wa_header-pin .
    p_country =   wa_header-Country  .

    CONCATENATE  '-' p_pin INTO p_pin SEPARATED BY space.
*      CONCATENATE p_add1  p_add2  p_dist p_city   p_state '-' p_pin  p_country INTO plant_add SEPARATED BY space.

    plant_name = wa_header-plant_name1.
    plant_gstin = wa_header-gstin_no.



**********************************************************************TransporterName

    SELECT SINGLE
    a~billingdocument,
    c~suppliername
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentpartner AS b ON b~BillingDocument = a~billingdocument
    LEFT JOIN I_Supplier AS c ON c~Supplier = b~Supplier
    WHERE a~billingdocument = @bill_doc
    AND b~PartnerFunction = 'SP'
    INTO @DATA(wa_tn)
    PRIVILEGED ACCESS.


*************************************************************************************


*********************************************************************************EMAIL


    SELECT SINGLE
    a~billingdocument ,
    d~emailaddress
    FROM i_BillingDocumentItem AS a
    LEFT JOIN i_plant  AS b ON a~plant = b~plant
    LEFT JOIN i_customer AS c ON b~PlantCustomer = c~customer
    LEFT JOIN i_addressemailaddress_2 AS d ON d~addressid = c~addressid
    WHERE a~billingdocument = @bill_doc
    INTO @DATA(wa_email)
    PRIVILEGED ACCESS.


**************************************************************************************


**********************************************************************BROKERName

    SELECT SINGLE
    a~billingdocument,
    c~suppliername
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentpartner AS b ON b~BillingDocument = a~billingdocument
    LEFT JOIN I_Supplier AS c ON c~Supplier = b~Supplier
    WHERE a~billingdocument = @bill_doc
    AND b~PartnerFunction = 'ES'
    INTO @DATA(wa_br)
    PRIVILEGED ACCESS.




*************************************************************************BROKERName END



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
  d~streetsuffixname1,
  d~streetsuffixname2
 FROM I_BillingDocument AS a
 LEFT JOIN i_billingdocumentpartner AS b ON b~billingdocument = a~billingdocument
 LEFT JOIN i_customer AS c ON c~customer = b~Customer
 LEFT JOIN i_address_2 AS d ON d~AddressID = c~AddressID
 LEFT JOIN i_regiontext AS e ON e~Region = c~Region AND e~Language = 'E' AND c~Country = e~Country
 LEFT JOIN i_countrytext AS f ON d~Country = f~Country
 WHERE b~partnerFunction = 'RE' AND  a~BillingDocument = @bill_doc
 INTO @DATA(wa_bill)
 PRIVILEGED ACCESS.

    DATA : Post_Ctry TYPE string.
    Post_Ctry =   wa_bill-PostalCode .
    CONCATENATE '-' Post_Ctry INTO Post_Ctry SEPARATED BY space.
*Concatenate   wa_bill-STREETSUFFIXNAME1 post_ctry into wa_bill-STREETSUFFIXNAME1 .

    DATA : Streetprefixxname TYPE string.
    Streetprefixxname = wa_bill-StreetPrefixName1 && '' && wa_bill-StreetPrefixName2 && ''.

    DATA : StreetSuffixname TYPE string.
    StreetSuffixname = wa_bill-StreetSuffixName1 && ' ' && wa_bill-StreetSuffixName2 && ''.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""SHIP TO  Address
*     SELECT SINGLE
*     d~streetname ,
*     d~streetprefixname1 ,
*     d~streetprefixname2 ,
*     d~cityname ,
*     d~region ,
*     d~postalcode ,
*     d~districtname ,
*     d~country  ,
*     d~housenumber ,
*     c~customername ,
*     a~soldtoparty ,
*     e~regionname ,
*     c~taxnumber3 ,
*     d~STREETSUFFIXNAME1,
*     d~STREETSUFFIXNAME2 ,
*     f~countryname
*    FROM I_BillingDocumentitem AS a
*    LEFT JOIN i_billingdocumentpartner AS b ON b~billingdocument = a~billingdocument
*    LEFT JOIN i_customer AS c ON c~customer = b~Customer
*    LEFT JOIN i_address_2 AS d ON d~AddressID = c~AddressID
*    LEFT JOIN I_RegionText AS e on e~Region = d~Region and e~Country = d~Country
*    LEFT JOIN i_countrytext AS f ON d~Country = f~Country
*    WHERE b~partnerFunction = 'WE'
*    and c~Language = 'E'
*    and a~BillingDocument = @bill_doc
*    INTO @DATA(wa_ship)
*    PRIVILEGED ACCESS.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""SHIP TO  Address LATEST

    SELECT SINGLE
    a~billingdocument ,
    c~businesspartnerfullname ,
    e~housenumber ,
    e~streetname ,
    e~streetprefixname1 ,
    e~streetprefixname2 ,
    e~streetsuffixname1 ,
    e~streetsuffixname2 ,
    e~cityname ,
    f~regionname ,
    g~countryname ,
    e~postalcode ,
    e~districtname ,
    d~taxnumber3
    FROM i_billingdocumentitem AS a
    LEFT JOIN i_deliverydocument AS b ON b~DeliveryDocument = a~ReferenceSDDocument
    LEFT JOIN i_businesspartner AS c ON c~BusinessPartner = b~ShipToParty
    LEFT JOIN i_customer AS d ON d~Customer = b~ShipToParty
    LEFT JOIN i_address_2 AS e ON e~AddressID = d~AddressID
    LEFT JOIN I_RegionText AS f ON f~Region = e~Region and f~Language = 'E' AND D~Country = F~Country
    LEFT JOIN I_CountryText AS g ON g~Country = e~Country
    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_ship2)
    PRIVILEGED ACCESS.

    """"""""""""""""""""""""""""""""""""""""""""""""""""""""""""SHIP TO  Address LATEST

    DATA : shipPost_Ctry TYPE string.
    shipPost_Ctry =  wa_ship2-PostalCode .
    CONCATENATE  '-'  shipPost_Ctry INTO shipPost_Ctry SEPARATED BY space.
*Concatenate   wa_ship-STREETSUFFIXNAME1 shipPost_Ctry into wa_ship-STREETSUFFIXNAME1 .

    DATA : ShipStreetprefixxname TYPE string.
    ShipStreetprefixxname = wa_ship2-StreetPrefixName1 && '' && wa_ship2-StreetPrefixName2 && ''.

    DATA : ShipStreetSuffixname TYPE string.
    ShipStreetSuffixname = wa_ship2-StreetSuffixName1 && ' ' && wa_ship2-StreetSuffixName2 && ''.

    DATA : wa_ad5 TYPE string.
    wa_ad5 = wa_bill-PostalCode.
    CONCATENATE wa_ad5 wa_bill-CityName  wa_bill-DistrictName INTO wa_ad5 SEPARATED BY space.

    DATA : wa_ad5_ship TYPE string.
    wa_ad5_ship = wa_ship2-PostalCode.
    CONCATENATE wa_ad5_ship wa_ship2-CityName  wa_ship2-DistrictName INTO wa_ad5_ship SEPARATED BY space.



**********************************************************************COMPANYDETAILS

**********************************************************************BILLTOPARTY FSSAINO
    SELECT SINGLE
    a~billingdocument ,
    b~bpidentificationnumber
    FROM i_billingdocument AS a
    LEFT JOIN I_BuPaIdentification AS b ON b~BusinessPartner = a~PayerParty
    WHERE a~BillingDocument = @bill_doc
    AND b~BPIdentificationType = 'FSSAI'
    INTO @DATA(wa_billfssai) .

**********************************************************************BILLTOPARTY FSSAINO END


**********************************************************************BILLTOPARTY FSSAINO

    SELECT SINGLE
    a~billingdocument ,
    d~bpidentificationnumber
    FROM i_billingdocument AS a
    LEFT JOIN  i_billingdocumentitem AS b ON b~billingdocument = a~billingdocument
    LEFT JOIN I_DeliveryDocument AS c ON c~DeliveryDocument = b~ReferenceSDDocument
    LEFT JOIN i_bupaidentification AS d ON d~BusinessPartner = c~ShipToParty
    WHERE a~BillingDocument = @bill_doc
    AND d~BPIdentificationType = 'FSSAI'
    INTO @DATA(wa_shipfssai).

**********************************************************************BILLTOPARTY FSSAINO END



    SELECT SINGLE
    a~billingdocument ,
    c~plantname ,
    d~streetname ,
    d~streetprefixname1 ,
    d~streetprefixname2 ,
    d~streetsuffixname1 ,
    d~streetsuffixname2 ,
    d~cityname ,
    d~region ,
    d~postalcode ,
    d~districtname ,
    d~country  ,
    d~housenumber ,
    e~regionname ,
    f~countryname
    FROM i_billingdocument AS a
    LEFT JOIN i_billingdocumentitem AS b ON b~BillingDocument = a~BillingDocument
    LEFT JOIN i_plant AS c ON c~Plant = b~Plant
    LEFT JOIN i_address_2 AS d ON d~AddressID = c~AddressID
    LEFT JOIN I_RegionText AS e ON e~Region = d~Region AND e~Country = d~Country
    LEFT JOIN i_countrytext AS f ON d~Country = f~Country
    WHERE a~billingdocument = @bill_doc
    INTO @DATA(wa_company)
    PRIVILEGED ACCESS.



    DATA : ship_post TYPE string.
    ship_post = wa_company-PostalCode.
    CONCATENATE '-' ship_post INTO ship_post SEPARATED BY space.
    DATA : CompStreetprefixxname TYPE string.
    CompStreetprefixxname = wa_company-StreetPrefixName1 && '' && wa_company-StreetPrefixName2 && ''.

    DATA : CompStreetSuffixname TYPE string.
    CompStreetSuffixname = wa_company-StreetSuffixName1 && ' ' && wa_company-StreetSuffixName2 && ''.

**********************************************************************COMPANYDETAILS END


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
*      g~conditionratevalue   ,  " i_per
*      g~conditionamount ,
*      g~conditionbasevalue,
*      g~conditiontype ,
      a~BILLINGTOBASEQUANTITYNMRTR ,
      a~itemNETweight ,
      j~referencesddocument ,
      a~Batch ,
      a~ItemWeightUnit

      FROM I_BillingDocumentItem AS a
      LEFT JOIN i_handlingunititem AS b ON a~referencesddocument = b~handlingunitreferencedocument
      LEFT JOIN i_handlingunitheader AS c ON b~handlingunitexternalid = c~handlingunitexternalid
      LEFT JOIN i_productdescription AS d ON d~product = a~product
      LEFT JOIN I_SalesDocumentItem AS e ON e~SalesDocument = a~SalesDocument AND e~salesdocumentitem = a~salesdocumentitem
      LEFT JOIN i_productplantbasic AS f ON a~Product = f~Product
*      LEFT JOIN i_billingdocumentitemprcgelmnt AS g ON g~BillingDocument = a~BillingDocument  AND g~BillingDocumentItem = a~BillingDocumentItem
      LEFT JOIN i_deliverydocumentitem AS h ON h~DeliveryDocument =  a~ReferenceSDDocument
      LEFT JOIN I_SalesOrderItem AS j ON j~SalesOrder = h~ReferenceSDDocument
      WHERE a~billingdocument = @bill_doc
      INTO TABLE  @DATA(it_item)
      PRIVILEGED ACCESS.


**********************************************************************CONDITION TYPE ZPR0
    SELECT billingdocument,
       billingdocumentitem,
       conditiontype,
       conditionratevalue,
       conditionbasevalue,
       conditionamount
FROM i_billingdocumentitemprcgelmnt
WHERE billingdocument = @bill_doc
INTO TABLE @DATA(it_conditions_ZPR0).

**********************************************************************CONDITION TYPE ZPR0 END

**********************************************************************CONDITION TYPE ZCIP

   SELECT billingdocument,
       billingdocumentitem,
       conditiontype,
       conditionratevalue,
       conditionbasevalue,
       conditionamount
FROM i_billingdocumentitemprcgelmnt
WHERE billingdocument = @bill_doc
INTO TABLE @DATA(it_conditions_ZCIP).



**********************************************************************CONDITION TYPE ZCIP END


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

    DATA: temp_add TYPE string.
    temp_add = wa_bill-postalcode.
    CONCATENATE temp_add wa_bill-CityName wa_bill-DistrictName INTO temp_add.

    DATA: temp_add_ship  TYPE string.
    temp_add_ship = wa_ship2-PostalCode.
    CONCATENATE temp_add wa_ship2-CityName wa_ship2-DistrictName INTO temp_add_ship.

    DATA(lv_xml) =
    |<Form>| &&
    |<BillingDocumentNode>| &&
    |<BillingDocument>{ wa_header-BillingDocument }</BillingDocument>| &&
    |<CurrentDate>{ lv_datecurr }</CurrentDate>| &&
    |<EWAYBILLNO>{ wa_header-ewaybillno }</EWAYBILLNO>| &&
    |<FSSAINO>{ wa_header-fssai_no }</FSSAINO>| &&
    |<SIGNEDQR>{ wa_header-signedqrcode }</SIGNEDQR>| &&
    |<AckNumber>{ wa_header-ackno }</AckNumber>| .

    IF wa_gatemain IS NOT INITIAL.
    DATA(LV_VEH) =
    |<VehicleNo>{ wa_gatemain-Vehicleno }</VehicleNo>|.
    CONCATENATE lv_xml lv_veh INTO lv_xml.
    ELSE .
    DATA(LV_VEH2) =
    |<VehicleNo>{ WA_IRN-vehiclenum }</VehicleNo>|.
    CONCATENATE lv_xml lv_veh INTO lv_xml.
    ENDIF.

    IF wa_gatemain IS NOT INITIAL.
    DATA(LV_LRNO) =
    |<LrNo>{ wa_gatemain-Lrno }</LrNo>| .
    CONCATENATE lv_xml lv_lrno INTO lv_xml.
    ELSE .
    DATA(LV_LRNO2) =
    |<LrNo>{ wa_irn-grno }</LrNo>| .
    CONCATENATE lv_xml lv_lrno2 INTO lv_xml.
    ENDIF.

    IF wa_gatemain IS NOT INITIAL.
    DATA(LV_TRNAME) =
    |<YY1_Transporter_Name_BDH>{ wa_gatemain-Transportername }</YY1_Transporter_Name_BDH>|.
    CONCATENATE lv_xml LV_TRNAME INTO lv_xml.
    ELSE .
    DATA(LV_TRNAME2) =
    |<YY1_Transporter_Name_BDH>{ wa_irn-transportername }</YY1_Transporter_Name_BDH>|.
    CONCATENATE lv_xml LV_TRNAME2 INTO lv_xml.
    ENDIF.

**********************************************************************LRDATE CONDENSE
*    DATA: lv_date      TYPE string,
*          lv_gatemain1 TYPE string.
*          lv_gatemain2 TYPE string.

IF wa_gatemain IS NOT INITIAL.
    " Check if lrdate is valid (not initial and not a default date)
*    IF wa_gatemain-lrdate IS NOT INITIAL AND wa_gatemain-lrdate <> '00000000' AND wa_gatemain-lrdate <> '00010101'.
      " Convert YYYYMMDD to DD/MM/YYYY format
      DATA(lv_date) = wa_gatemain-lrdate.
      CONDENSE lv_date NO-GAPS.  " Remove any spaces
      lv_date = lv_date+6(2) && '/' && lv_date+4(2) && '/' && lv_date(4).

      " Generate XML tag with formatted date
      DATA(lv_gatemain) =
      |<LrDate>{ lv_date }</LrDate>|.
      CONCATENATE lv_xml lv_gatemain INTO lv_xml . " RESPECTING BLANKS.
*    ELSE.
      " Generate XML tag with an empty LrDate
*      lv_gatemain2 = |<LrDate></LrDate>|.
*      CONCATENATE lv_xml lv_gatemain2 INTO lv_xml RESPECTING BLANKS.
*    ENDIF.
ELSE .
DATA(lv_date2) = wa_irn-grdate.
      CONDENSE lv_date2 NO-GAPS.  " Remove any spaces
      lv_date2 = lv_date2+6(2) && '/' && lv_date2+4(2) && '/' && lv_date2(4).

       DATA(lv_gatemain2) =
      |<LrDate>{ lv_date2 }</LrDate>|.
      CONCATENATE lv_xml lv_gatemain2 INTO lv_xml .

ENDIF.

*DATA: lv_date      TYPE string,
*      lv_gatemain1 TYPE string,
*      lv_gatemain2 TYPE string.
*
*" Check if lrdate is valid (not initial and not a default date)
*IF wa_gatemain-lrdate IS NOT INITIAL AND wa_gatemain-lrdate <> '00000000' AND wa_gatemain-lrdate <> '00010101'.
*
*    " Determine the format based on length (YYYYMMDD = 8, YYYY-MM-DD hh:mm:ss > 8)
*    IF strlen( wa_gatemain-lrdate ) = 8.
*        " Convert YYYYMMDD to DD/MM/YYYY format
*        lv_date = wa_gatemain-lrdate+6(2) && '/' && wa_gatemain-lrdate+4(2) && '/' && wa_gatemain-lrdate(4).
*    ELSE.
*        " Convert YYYY-MM-DD hh:mm:ss to DD/MM/YYYY format
*        lv_date = wa_gatemain-lrdate+8(2) && '/' && wa_gatemain-lrdate+5(2) && '/' && wa_gatemain-lrdate(4).
*    ENDIF.
*
*    " Generate XML tag with formatted date
*    lv_gatemain1 = |<LrDate>{ lv_date }</LrDate>|.
*    CONCATENATE lv_xml lv_gatemain1 INTO lv_xml RESPECTING BLANKS.
*ELSE.
*    " Generate XML tag with an empty LrDate
*    lv_gatemain2 = |<LrDate></LrDate>|.
*    CONCATENATE lv_xml lv_gatemain2 INTO lv_xml RESPECTING BLANKS.
*ENDIF.


**********************************************************************LRDATE CONDENSE END

**********************************************************************EWAYBILLDATE CONDENSE

    DATA: lv2_date     TYPE string,
          lv_gatemain3 TYPE string,
          lv_gatemain4 TYPE string.

    " Check if ewaydate is valid (not initial and not a default date)
    IF wa_header-ewaydate IS NOT INITIAL AND wa_header-ewaydate <> '00000000' AND wa_header-ewaydate <> '00010101'.

      " Determine the format based on length (YYYYMMDD = 8, YYYY-MM-DD hh:mm:ss > 8)
      IF strlen( wa_header-ewaydate ) = 8.
        " Convert YYYYMMDD to DD/MM/YYYY format
        lv2_date = wa_header-ewaydate+6(2) && '/' && wa_header-ewaydate+4(2) && '/' && wa_header-ewaydate(4).
      ELSE.
        " Convert YYYY-MM-DD hh:mm:ss to DD/MM/YYYY format
        lv2_date = wa_header-ewaydate+8(2) && '/' && wa_header-ewaydate+5(2) && '/' && wa_header-ewaydate(4).
      ENDIF.

      " Generate XML tag with formatted date
      lv_gatemain3 = |<EWAYBILLDATE>{ lv2_date }</EWAYBILLDATE>|.
      CONCATENATE lv_xml lv_gatemain3 INTO lv_xml RESPECTING BLANKS.
    ELSE.
      " Generate XML tag with an empty EWAYBILLDATE
      lv_gatemain4 = |<EWAYBILLDATE></EWAYBILLDATE>|.
      CONCATENATE lv_xml lv_gatemain4 INTO lv_xml RESPECTING BLANKS.
    ENDIF.




**********************************************************************EWAYBILLDATE CONDENSE END

**********************************************************************ACKDATE CONDENSE
*DATA: lv3_date      TYPE string,
*      lv_gatemain5  TYPE string,
*      lv_gatemain6  TYPE string.
*
*" Check if ackdate is valid (not initial and not a default date)
*IF wa_header-ackdate IS NOT INITIAL AND wa_header-ackdate <> '00000000' AND wa_header-ackdate <> '00010101'.
*    " Extract YYYY, MM, and DD from the format 'YYYY-MM-DD hh:mm:ss'
*    lv3_date = wa_header-ackdate+8(2) && '/' && wa_header-ackdate+5(2) && '/' && wa_header-ackdate(4).
*
*    " Generate XML tag with formatted date
*    lv_gatemain5 = |<AckDate>{ lv3_date }</AckDate>|.
*    CONCATENATE lv_xml lv_gatemain5 INTO lv_xml RESPECTING BLANKS.
*ELSE.
*    " Generate XML tag with an empty AckDate
*    lv_gatemain6 = |<AckDate></AckDate>|.
*    CONCATENATE lv_xml lv_gatemain6 INTO lv_xml RESPECTING BLANKS.
*ENDIF.

    DATA: lv3_date     TYPE string,
          lv_gatemain5 TYPE string,
          lv_gatemain6 TYPE string.

    " Check if ackdate is valid (not initial and not a default date)
    IF wa_header-ackdate IS NOT INITIAL AND wa_header-ackdate <> '00000000' AND wa_header-ackdate <> '00010101'.

      " Determine the format based on length (YYYYMMDD = 8, YYYY-MM-DD hh:mm:ss > 8)
      IF strlen( wa_header-ackdate ) = 8.
        " Convert YYYYMMDD to DD/MM/YYYY format
        lv3_date = wa_header-ackdate+6(2) && '/' && wa_header-ackdate+4(2) && '/' && wa_header-ackdate(4).
      ELSE.
        " Convert YYYY-MM-DD hh:mm:ss to DD/MM/YYYY format
        lv3_date = wa_header-ackdate+8(2) && '/' && wa_header-ackdate+5(2) && '/' && wa_header-ackdate(4).
      ENDIF.

      " Generate XML tag with formatted date
      lv_gatemain5 = |<AckDate>{ lv3_date }</AckDate>|.
      CONCATENATE lv_xml lv_gatemain5 INTO lv_xml RESPECTING BLANKS.
    ELSE.
      " Generate XML tag with an empty AckDate
      lv_gatemain6 = |<AckDate></AckDate>|.
      CONCATENATE lv_xml lv_gatemain6 INTO lv_xml RESPECTING BLANKS.
    ENDIF.



**********************************************************************ACKDATE CONDENSE END



    DATA(lv_xml2) =
    |<TransporterMode>{ wa_gatemain-Transportmode }</TransporterMode>| &&
*    |<YY1_EmailAddress_BDH>{ wa_email-EmailAddress }</YY1_EmailAddress_BDH>| &&
    |<BillingDate>{ wa_header-billingdate }</BillingDate>| &&
    |<BankName>{ wa_bank-bank_details }</BankName>| &&
    |<AccNo>{ wa_bank-acoount_number }</AccNo>| &&
    |<IFSCNo>{ wa_bank-ifsc_code }</IFSCNo>| &&
    |<DocumentReferenceID>{ wa_header-DocumentReferenceID }</DocumentReferenceID>| &&
    |<PurchaseOrderByCustomer>{ wa_header-PurchaseOrderByCustomer }</PurchaseOrderByCustomer>| &&
    |<YY1_Broker_BDH>{ wa_br-SupplierName }</YY1_Broker_BDH>| &&
    |<YY1_Phone_Number_BDH>{ wa_header-TelephoneNumber1 }</YY1_Phone_Number_BDH>| &&
    |<YY1_SHIPTO_PHONENUMBER_BDH>{ wa_sp-TelephoneNumber1 }</YY1_SHIPTO_PHONENUMBER_BDH>| &&
    |<Irn>{ wa_header-irnno }</Irn>| &&
*    |<YY1_PLANT_COM_ADD_BDH>{ plant_add }</YY1_PLANT_COM_ADD_BDH>| &&
    |<YY1_PLANT_COM_NAME_BDH>{ plant_name }</YY1_PLANT_COM_NAME_BDH>| &&
    |<YY1_PLANT_COM_GSTIN_NO_BDH>{ plant_gstin }</YY1_PLANT_COM_GSTIN_NO_BDH>| &&
    |<Supplier>| &&
    |<CompanyCode>{ wa_header-SalesOrganization }</CompanyCode>| &&
    |<AddressLine1Text>{ wa_company-PlantName }</AddressLine1Text>| &&
    |<AddressLine2Text>{ CompStreetprefixxname }</AddressLine2Text>| &&
    |<AddressLine3Text>{ wa_company-StreetName }</AddressLine3Text>| &&
    |<AddressLine4Text>{ CompStreetSuffixname }</AddressLine4Text>| &&
    |<AddressLine5Text>{ wa_company-CityName }</AddressLine5Text>| &&
    |<AddressLine6Text>{ wa_company-RegionName }</AddressLine6Text>| &&
    |<AddressLine7Text>{ wa_company-CountryName }</AddressLine7Text>| &&
    |<AddressLine8Text>{ ship_post }</AddressLine8Text>| &&
    |</Supplier>| &&
    |<PlantAdd>| &&
    |<AddressLine1Text>{ p_add1 }</AddressLine1Text>| &&
    |<AddressLine2Text>{ p_add2 }</AddressLine2Text>| &&
    |<AddressLine3Text>{ p_dist }</AddressLine3Text>| &&
    |<AddressLine4Text>{ p_city }</AddressLine4Text>| &&
    |<AddressLine5Text>{ p_state }</AddressLine5Text>| &&
    |<AddressLine6Text>{ p_country }</AddressLine6Text>| &&
    |<AddressLine7Text>{ p_pin }</AddressLine7Text>| &&
    |<AddressLine8Text></AddressLine8Text>| &&
    |</PlantAdd>| &&
    |<Incoterms>| &&
    |<Incoterms>{ wa_header-IncotermsClassification }</Incoterms>| &&
    |<IncotermsLocation1>{ wa_header-IncotermsLocation1 }</IncotermsLocation1>| &&
    |</Incoterms>| &&
    |<TaxationTerms>| &&
    |<IN_ShipToPtyGSTIdnNmbr>{ wa_ship2-TaxNumber3 }</IN_ShipToPtyGSTIdnNmbr>| &&
    |</TaxationTerms>| &&
    |<ShipToParty>| &&
    |<RegionName>{ wa_ship2-RegionName }</RegionName>| &&
    |<Partner>{ wa_spart-ShipToParty }</Partner>| &&
    |<AddressLine1Text>{ wa_ship2-BusinessPartnerFullName }</AddressLine1Text>| &&
    |<ShipToFssaiNo>{ wa_shipfssai-BPIdentificationNumber }</ShipToFssaiNo>| &&
    |</ShipToParty>| &&
*    |<Supplier>| &&

*    |</Supplier>| &&
    |<Company>| &&
    |<CompanyName>{ wa_header-SalesOrganizationName }</CompanyName>| &&
    |</Company>| &&
*12.03    |<YY1_dodatebd_BDH>{ wa_header-YY1_DODate_SDH }</YY1_dodatebd_BDH>| &&
*12.03    |<YY1_dono_bd_BDH>{ wa_header-YY1_DONo_SDH }</YY1_dono_bd_BDH>| &&
*    |<Plant>{ wa_header-Plant }</Plant>| &&
*    |<RegionName>{ wa_header-state_name }</RegionName>| &&
    |<BillToParty>| &&
    |<AddressLine1Text>{ wa_bill-CustomerName }</AddressLine1Text>| &&
    |<AddressLine2Text>{ Streetprefixxname }</AddressLine2Text>| &&
    |<AddressLine3Text>{ wa_bill-StreetName }</AddressLine3Text>| &&
    |<AddressLine4Text>{ StreetSuffixname }</AddressLine4Text>| &&
    |<AddressLine5Text>{ wa_bill-CityName }</AddressLine5Text>| &&
    |<AddressLine6Text>{ wa_bill-RegionName }</AddressLine6Text>| &&
    |<AddressLine7Text>{ wa_bill-CountryName }</AddressLine7Text>| &&
    |<AddressLine8Text>{ Post_Ctry }</AddressLine8Text>| &&
    |<BillToFssaiNo>{ wa_billfssai-bpidentificationnumber }</BillToFssaiNo>| &&
*    |<Region>{ wa_bill-Region }</Region>| &&
    |<FullName>{ wa_bill-CustomerName }</FullName>| &&   " done
*12.03    |<Partner>{ wa_header-YY1_DONo_SDH }</Partner>| &&
    |<RegionName>{ wa_bill-RegionName }</RegionName>| &&
    |<Partner>{ wa_bpart-PayerParty }</Partner>| &&
    |</BillToParty>| &&
    |<Items>|.

    CONCATENATE lv_xml lv_xml2 INTO lv_xml.


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
      |<Batch>{ wa_item-Batch }</Batch>| &&
      |<SalesContract>{ wa_item-ReferenceSDDocument }</SalesContract>| &&
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
      |</ItemPricingConditions>|.
      CONCATENATE lv_xml lv_item3_xml INTO lv_xml.

**********************************************************************WEIGHT(KG)
*      SELECT FROM I_BILLINGDOCUMENTITEM AS A
*      FIELDS A~BillingDocument , A~BillingQuantity , A~ItemNETWeight , A~ItemWeightUnit
*      WHERE A~BillingDocument = @bill_doc
*      INTO TABLE @DATA(IT_WEIGHT).
*
*      LOOP AT IT_WEIGHT INTO DATA(WA_WEIGHT).


*      DATA : BILLQT TYPE P DECIMALS 3.
      DATA : ITMGRW TYPE P DECIMALS 3.
      DATA : BILLGRW TYPE P DECIMALS 3.
      DATA : BILLGRW2 TYPE P DECIMALS 3.
*      BILLQT = WA_ITEM-BillingQuantity .
      ITMGRW = WA_ITEM-itemNETweight .
      BILLGRW =  ITMGRW .
      BILLGRW2 = ITMGRW * 1000 .

      IF WA_item-ItemWeightUnit = 'KG' .
        DATA(LV_WEIGHT) =
            |<NetWeight>{ BILLGRW }</NetWeight>| .
      CONCATENATE lv_xml  lv_weight INTO lv_xml.

      ELSE.
        DATA(LV_WEIGHT2) =
            |<NetWeight>{ BILLGRW2 }</NetWeight>| .
      CONCATENATE lv_xml  lv_weight2 INTO lv_xml.
      CLEAR :  billgrw , itmgrw  , billgrw2 . "BILLQT .
      ENDIF.
*      ENDLOOP.

**********************************************************************WEIGHT(KG) END

**********************************************************************RATE PER UOM ZPR0


READ TABLE it_conditions_zpr0 INTO DATA(wa_cond)
    WITH KEY billingdocument     = wa_item-billingdocument
             billingdocumentitem = wa_item-billingdocumentitem
             conditiontype       = 'ZPR0'.
IF sy-subrc = 0.
    DATA : lv_ZPR0  TYPE p DECIMALS 2.
    lv_ZPR0 = wa_item-BILLINGTOBASEQUANTITYNMRTR * wa_cond-conditionratevalue.

    DATA(lv_item_rate) = |<RATEPERUOM>{ lv_ZPR0 }</RATEPERUOM>|.
    CONCATENATE lv_xml lv_item_rate INTO lv_xml.


  ELSE.
    DATA(lv_item_rate2) = |<RATEPERUOM></RATEPERUOM>|.
    CONCATENATE lv_xml lv_item_rate2 INTO lv_xml.
  ENDIF.

**********************************************************************RATE PER UOM ZPR0 END

**********************************************************************RATE PER UOM ZCIP END

READ TABLE it_conditions_zcip INTO DATA(wa_cond_ZCIP)
    WITH KEY billingdocument     = wa_item-billingdocument
             billingdocumentitem = wa_item-billingdocumentitem
             conditiontype       = 'ZCIP'.
IF sy-subrc = 0.
    DATA : lv_ZCIP  TYPE p DECIMALS 2.
    lv_zcip = wa_item-BILLINGTOBASEQUANTITYNMRTR * wa_cond_zcip-conditionratevalue.

    DATA(lv_item_rate_ZCIP) = |<ZCIPRATEPERUOM>{ lv_zcip }</ZCIPRATEPERUOM>|.
    CONCATENATE lv_xml lv_item_rate_ZCIP INTO lv_xml.

  ELSE.
    DATA(lv_item_rate_zcip2) = |<ZCIPRATEPERUOM></ZCIPRATEPERUOM>|.
    CONCATENATE lv_xml lv_item_rate_zcip2 INTO lv_xml.
  ENDIF.


**********************************************************************RATE PER UOM ZCIP END



*DATA : CONTYPE TYPE STRING .
*DATA : CONBASEVAL2 TYPE P DECIMALS 2.
*DATA : CONDRATEVAL2 TYPE P DECIMALS 2.
*DATA : CONDMULVAL2 TYPE P DECIMALS 2.
*CONBASEVAL2 = wa_item-BILLINGTOBASEQUANTITYNMRTR .
*CONDRATEVAL2 = wa_cond-ConditionRateValue .
*CONDMULVAL2 = conbaseval2 * condrateval2 .
*CONTYPE = wa_cond-ConditionType.
*
*IF contype = 'ZPR0' .
*  DATA(LV_ITEM_RATE) =
*   |<RATEPERUOM>{ CONDMULVAL2 }</RATEPERUOM>|.
*CONCATENATE   lv_xml lv_item_rate INTO lv_xml.
*
*ELSE .
*    DATA(LV_ITEM_RATE2) =
*   |<RATEPERUOM></RATEPERUOM>|.
*CONCATENATE   lv_xml lv_item_rate2 INTO lv_xml.
*CLEAR : contype , CONBASEVAL2 , CONDRATEVAL2 , CONDMULVAL2   .
*ENDIF.
*DATA : CONBASEVAL3 TYPE P DECIMALS 2.
*DATA : CONDRATEVAL3 TYPE P DECIMALS 2.
*DATA : CONDMULVAL3 TYPE P DECIMALS 2.
*CONBASEVAL3 = wa_item-BILLINGTOBASEQUANTITYNMRTR .
*CONDRATEVAL3 = wa_item-ConditionRateValue .
*CONDMULVAL3 = conbaseval3 * condrateval3 .
*
*ELSEIF  WA_item-ConditionType = 'ZCIP' .
*  DATA(LV_ITEM_RATE2) =
*   |<STORATEPERUOM>{ CONDMULVAL3 }</STORATEPERUOM>|.
*CONCATENATE   lv_xml LV_ITEM_RATE2 INTO lv_xml.
*CLEAR :  CONBASEVAL3 , CONDRATEVAL3 , CONDMULVAL3   .

*ENDIF.

**********************************************************************RATE PER UOM END


**********************************************************************ZCIP RATE PER UOM

*DATA : CONBASEVAL3 TYPE P DECIMALS 2.
*DATA : CONDRATEVAL3 TYPE P DECIMALS 2.
*DATA : CONDMULVAL3 TYPE P DECIMALS 2.
*CONBASEVAL3 = wa_item-BILLINGTOBASEQUANTITYNMRTR .
*CONDRATEVAL3 = wa_item-ConditionRateValue .
*CONDMULVAL3 = conbaseval3 * condrateval3 .
*
*ELSEIF  WA_item-ConditionType = 'ZCIP' .
*  DATA(LV_ITEM_RATE2) =
*   |<STORATEPERUOM>{ CONDMULVAL3 }</STORATEPERUOM>|.
*CONCATENATE   lv_xml LV_ITEM_RATE2 INTO lv_xml.
*CLEAR :  CONBASEVAL3 , CONDRATEVAL3 , CONDMULVAL3   .
*ENDIF.


**********************************************************************ZCIP RATE PER UOM END



      DATA(LV_ITEM8_XML) =
      |</BillingDocumentItemNode>|.
      CONCATENATE lv_xml lv_item8_xml INTO lv_xml.


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
    |<AddressLine2Text>{ ShipStreetprefixxname }</AddressLine2Text>| &&
    |<AddressLine3Text>{ wa_ship2-StreetName }</AddressLine3Text>| &&
    |<AddressLine4Text>{ ShipStreetSuffixname }</AddressLine4Text>| &&
    |<AddressLine5Text>{ wa_ship2-CityName }</AddressLine5Text>| &&
    |<AddressLine6Text>{ wa_ship2-RegionName }</AddressLine6Text>| &&
    |<AddressLine7Text>{ wa_ship2-CountryName }</AddressLine7Text>| &&
    |<AddressLine8Text>{ shipPost_Ctry }</AddressLine8Text>| &&
    |<FullName>{ wa_bill-Region }</FullName>| &&
    |<RegionName>{ wa_ship2-RegionName }</RegionName>| &&
    |</ShipToParty>|.

    CONCATENATE lv_xml lv_shiptoparty INTO lv_xml.

*    DATA(lv_supplier) =
*    |<Supplier>| &&
*    |<RegionName></RegionName>| &&                " pending
*    |</Supplier>|.
*    CONCATENATE lv_xml lv_supplier INTO lv_xml.

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
    CLEAR wa_ship2.
    CLEAR wa_header.



    REPLACE ALL OCCURRENCES OF '&' IN lv_xml WITH 'and' .



*    REPLACE ALL OCCURRENCES OF '&' IN lv_xml WITH 'and'.
*    REPLACE ALL OCCURRENCES OF '<=' IN lv_xml WITH 'let'.
*    REPLACE ALL OCCURRENCES OF '>=' IN lv_xml WITH 'get'.

    out->write( lv_xml ).



ENDMETHOD.

ENDCLASS.

