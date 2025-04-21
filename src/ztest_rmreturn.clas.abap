CLASS ztest_rmreturn DEFINITION
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



CLASS ZTEST_RMRETURN IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

bill_doc = '0090000540'.
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
           p_pin type string.



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
    l~SALESORGANIZATIONNAME ,
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
    LEFT JOIN I_SalesOrganizationText as l on l~SalesOrganization = b~SalesOrganization
    LEFT JOIN I_Customer as m on m~Customer = a~payerparty

    WHERE a~BillingDocument = @bill_doc
    INTO @DATA(wa_header).


**********************************************************************BILLTOPARTNER

SELECT SINGLE
a~billingdocument ,
a~payerparty
from i_billingdocument as a
where a~BillingDocument = @bill_doc
into @data(wa_bpart).

SHIFT wa_bpart-PayerParty LEFT DELETING LEADING '0'.


***********************************************************************************

**********************************************************************SHIPTOPARTNER

SELECT SINGLE
a~billingdocument ,
b~shiptoparty
FROM I_BillingDocumentItem as a
left join I_DeliveryDocument as b on b~DeliveryDocument = a~ReferenceSDDocument
where a~BillingDocument = @bill_doc
into @data(wa_spart) .


SHIFT wa_spart-ShipToParty LEFT DELETING LEADING '0'.

***********************************************************************************




**********************************************************************SHIPTOPHONE

Select Single
c~telephonenumber1,
a~billingdocument
from i_BillingDocumentitem as a
left join i_deliverydocument as b on b~deliverydocument = a~referencesddocument
left join i_customer as c on c~Customer = b~shiptoparty
WHERE a~billingdocument = @bill_doc
INTO @DATA(wa_sp) PRIVILEGED access.


************************************************************************************
*    SELECT single from zgateentryheader as a
*        left join zgateentrylines as b on a~gateentryno = b~gateentryno
*      fields a~gateentryno, b~documentno
*        where a~gateentryno = b~gateentryno
*        into @data(wa_gateno).

**********************************************************************LR NO & VEHICLENO & TRANSPORTMODE & LR DATE

    select single
    a~billingdocument ,
    b~referencesddocument
    from i_billingdocument as a
    left join i_billingdocumentitem as b on b~billingdocument = a~billingdocument
    where a~BillingDocument = @bill_doc
    into @data(wa_lrvntm).

    SHIFT wa_lrvntm-ReferenceSDDocument LEFT DELETING LEADING '0'.

    select single
    d~vehicleno ,
    d~LRNo ,
    d~transportmode ,
    d~transportername ,
    d~lrdate
    from i_billingdocument as a
    left join i_billingdocumentitem as b on b~billingdocument = a~billingdocument
    left join zr_gateentrylines as c on c~Documentno = @wa_lrvntm-ReferenceSDDocument
    left join ZR_GateEntryHeader as d on d~Gateentryno = c~Gateentryno
    where a~billingdocument = @bill_doc
    into @data(wa_gatemain).





***********************************************************************LR NO & VEHICLENO & TRANSPORTMODE & LR DATE END
      p_add1 = wa_header-address1 .
      p_add2 = wa_header-address2 .
      p_dist = wa_header-district .
      p_city = wa_header-city .
      p_state = wa_header-state_name .
      p_pin =  wa_header-pin .
      p_country =   wa_header-Country  .

CONCATENATE  '-' p_pin into p_pin SEPARATED BY space.
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
PRIVILEGED access.


*************************************************************************************


*********************************************************************************EMAIL


Select Single
a~billingdocument ,
d~emailaddress
from i_BillingDocumentItem as a
left join i_plant  as b on a~plant = b~plant
left join i_customer as c on b~PlantCustomer = c~customer
left join i_addressemailaddress_2 as d on d~addressid = c~addressid
WHERE A~billingdocument = @bill_doc
INTO @DATA(wa_email)
PRIVILEGED access.


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
PRIVILEGED access.




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

Data : Post_Ctry type string.
Post_Ctry =   wa_bill-PostalCode .
CONCATENATE '-' Post_Ctry into Post_Ctry SEPARATED by SPACE.
*Concatenate   wa_bill-STREETSUFFIXNAME1 post_ctry into wa_bill-STREETSUFFIXNAME1 .

Data : Streetprefixxname type string.
Streetprefixxname = wa_bill-StreetPrefixName1 && '' && wa_bill-StreetPrefixName2 && ''.

Data : StreetSuffixname type string.
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
    A~BILLINGDOCUMENT ,
    C~BUSINESSPARTNERFULLNAME ,
    E~HOUSENUMBER ,
    E~STREETNAME ,
    E~STREETPREFIXNAME1 ,
    E~STREETPREFIXNAME2 ,
    E~STREETSUFFIXNAME1 ,
    E~STREETSUFFIXNAME2 ,
    E~CITYNAME ,
    F~REGIONNAME ,
    G~COUNTRYNAME ,
    E~POSTALCODE ,
    E~DISTRICTNAME ,
    D~TAXNUMBER3
    FROM I_BILLINGDOCUMENTITEM AS A
    LEFT JOIN I_DELIVERYDOCUMENT AS B ON B~DeliveryDocument = A~ReferenceSDDocument
    LEFT JOIN I_BUSINESSPARTNER AS C ON C~BusinessPartner = B~ShipToParty
    LEFT JOIN I_CUSTOMER AS D ON D~Customer = B~ShipToParty
    LEFT JOIN I_ADDRESS_2 AS E ON E~AddressID = D~AddressID
    LEFT JOIN I_RegionText AS F ON F~Region = E~Region
    LEFT JOIN I_CountryText AS G ON G~Country = E~Country
    WHERE A~BillingDocument = @bill_doc
    INTO @DATA(WA_SHIP2)
    PRIVILEGED ACCESS.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""SHIP TO  Address LATEST

Data : shipPost_Ctry type string.
shipPost_Ctry =  WA_SHIP2-PostalCode .
CONCATENATE  '-'  shipPost_Ctry into shipPost_Ctry SEPARATED by space.
*Concatenate   wa_ship-STREETSUFFIXNAME1 shipPost_Ctry into wa_ship-STREETSUFFIXNAME1 .

Data : ShipStreetprefixxname type string.
ShipStreetprefixxname = WA_SHIP2-StreetPrefixName1 && '' && WA_SHIP2-StreetPrefixName2 && ''.

Data : ShipStreetSuffixname type string.
ShipStreetSuffixname = WA_SHIP2-StreetSuffixName1 && ' ' && WA_SHIP2-StreetSuffixName2 && ''.

      DATA : wa_ad5 TYPE string.
      wa_ad5 = wa_bill-PostalCode.
      CONCATENATE wa_ad5 wa_bill-CityName  wa_bill-DistrictName INTO wa_ad5 SEPARATED BY space.

      DATA : wa_ad5_ship TYPE string.
      wa_ad5_ship = WA_SHIP2-PostalCode.
      CONCATENATE wa_ad5_ship WA_SHIP2-CityName  WA_SHIP2-DistrictName INTO wa_ad5_ship SEPARATED BY space.



**********************************************************************COMPANYDETAILS

**********************************************************************BILLTOPARTY FSSAINO
SELECT SINGLE
a~billingdocument ,
b~BPIDENTIFICATIONNUMBER
from i_billingdocument as a
left join I_BuPaIdentification as b on b~BusinessPartner = a~PayerParty
where a~BillingDocument = @bill_doc
and b~BPIdentificationType = 'FSSAI'
into @data(wa_billfssai) .

**********************************************************************BILLTOPARTY FSSAINO END


**********************************************************************BILLTOPARTY FSSAINO

SELECT SINGLE
a~billingdocument ,
d~BPIDENTIFICATIONNUMBER
from i_billingdocument as a
left join  i_billingdocumentitem as b on b~billingdocument = a~billingdocument
left join I_DeliveryDocument as c on c~DeliveryDocument = b~ReferenceSDDocument
left join i_bupaidentification as d on d~BusinessPartner = c~ShipToParty
where a~BillingDocument = @bill_doc
and d~BPIdentificationType = 'FSSAI'
into @data(wa_shipfssai).

**********************************************************************BILLTOPARTY FSSAINO END



Select single
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
from i_billingdocument as a
left join i_billingdocumentitem as b on b~BillingDocument = a~BillingDocument
left join i_plant as c on c~Plant = b~Plant
left join i_address_2 as d on d~AddressID = c~AddressID
LEFT JOIN I_RegionText AS e on e~Region = d~Region and e~Country = d~Country
LEFT JOIN i_countrytext AS f ON d~Country = f~Country
where a~billingdocument = @bill_doc
into @data(wa_company)
PRIVILEGED ACCESS.



data : ship_post type string.
ship_post = wa_company-PostalCode.
CONCATENATE '-' ship_post into ship_post SEPARATED BY space.
Data : CompStreetprefixxname type string.
CompStreetprefixxname = wa_company-StreetPrefixName1 && '' && wa_company-StreetPrefixName2 && ''.

Data : CompStreetSuffixname type string.
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
        g~conditionratevalue   ,  " i_per
        g~conditionamount ,
        g~conditionbasevalue,
        g~conditiontype ,
        a~itemgrossweight ,
        j~referencesddocument ,
        a~Batch

        FROM I_BillingDocumentItem AS a
        LEFT JOIN i_handlingunititem AS b ON a~referencesddocument = b~handlingunitreferencedocument
        LEFT JOIN i_handlingunitheader AS c ON b~handlingunitexternalid = c~handlingunitexternalid
        LEFT JOIN i_productdescription AS d ON d~product = a~product
        LEFT JOIN I_SalesDocumentItem AS e ON e~SalesDocument = a~SalesDocument AND e~salesdocumentitem = a~salesdocumentitem
        LEFT JOIN i_productplantbasic AS f ON a~Product = f~Product
        LEFT JOIN i_billingdocumentitemprcgelmnt AS g ON g~BillingDocument = a~BillingDocument AND g~BillingDocumentItem = a~BillingDocumentItem
        LEFT JOIN i_deliverydocumentitem AS h on h~DeliveryDocument =  a~ReferenceSDDocument
        LEFT JOIN I_SalesOrderItem AS J on J~SalesOrder = h~ReferenceSDDocument
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

    data: temp_add_ship  type string.
    temp_add_ship = wa_ship2-PostalCode.
    CONCATENATE temp_add wa_ship2-CityName wa_ship2-DistrictName into temp_add_ship.

    DATA(lv_xml) =
    |<Form>| &&
    |<BillingDocumentNode>| &&
    |<BillingDocument>{ wa_header-BillingDocument }</BillingDocument>| &&
    |<EWAYBILLNO>{ wa_header-ewaybillno }</EWAYBILLNO>| &&
    |<FSSAINO>{ wa_header-fssai_no }</FSSAINO>| &&
    |<SIGNEDQR>{ wa_header-signedqrcode }</SIGNEDQR>| &&
    |<AckNumber>{ wa_header-ackno }</AckNumber>| &&
    |<VehicleNo>{ wa_gatemain-Vehicleno }</VehicleNo>| &&
    |<LrNo>{ wa_gatemain-Lrno }</LrNo>| .

**********************************************************************LRDATE CONDENSE
DATA: lv_date      TYPE string,
      lv_gatemain1 TYPE string,
      lv_gatemain2 TYPE string.

" Check if lrdate is valid (not initial and not a default date)
IF wa_gatemain-lrdate IS NOT INITIAL AND wa_gatemain-lrdate <> '00000000' AND wa_gatemain-lrdate <> '00010101'.
    " Convert YYYYMMDD to DD/MM/YYYY format
    lv_date = wa_gatemain-lrdate.
    CONDENSE lv_date NO-GAPS.  " Remove any spaces
    lv_date = lv_date+6(2) && '/' && lv_date+4(2) && '/' && lv_date(4).

    " Generate XML tag with formatted date
    lv_gatemain1 = |<LrDate>{ lv_date }</LrDate>|.
    CONCATENATE lv_xml lv_gatemain1 INTO lv_xml RESPECTING BLANKS.
ELSE.
    " Generate XML tag with an empty LrDate
    lv_gatemain2 = |<LrDate></LrDate>|.
    CONCATENATE lv_xml lv_gatemain2 INTO lv_xml RESPECTING BLANKS.
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

DATA: lv2_date      TYPE string,
      lv_gatemain3  TYPE string,
      lv_gatemain4  TYPE string.

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

DATA: lv3_date      TYPE string,
      lv_gatemain5  TYPE string,
      lv_gatemain6  TYPE string.

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



    Data(lv_xml2) =
    |<TransporterMode>{ wa_gatemain-Transportmode }</TransporterMode>| &&
    |<YY1_Transporter_Name_BDH>{ wa_gatemain-Transportername }</YY1_Transporter_Name_BDH>| &&
*    |<YY1_EmailAddress_BDH>{ wa_email-EmailAddress }</YY1_EmailAddress_BDH>| &&
    |<BillingDate>{ wa_header-billingdate }</BillingDate>| &&
    |<BankName></BankName>| &&
    |<AccNo></AccNo>| &&
    |<IFSCNo></IFSCNo>| &&
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
    |<BillToFssaiNo>{ wa_billfssai-BPIDENTIFICATIONNUMBER }</BillToFssaiNo>| &&
*    |<Region>{ wa_bill-Region }</Region>| &&
    |<FullName>{ wa_bill-CustomerName }</FullName>| &&   " done
*12.03    |<Partner>{ wa_header-YY1_DONo_SDH }</Partner>| &&
    |<RegionName>{ wa_bill-RegionName }</RegionName>| &&
    |<Partner>{ wa_bpart-PayerParty }</Partner>| &&
    |</BillToParty>| &&
    |<Items>|.

    CONCATENATE lv_xml lv_xml2 into lv_xml.


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
      |<NetWeight>{ wa_item-ItemGrossWeight }</NetWeight>| &&
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



    REPLACE ALL OCCURRENCES OF '&' IN lv_xml WITH 'and'.
    REPLACE ALL OCCURRENCES OF '<=' IN lv_xml WITH 'let'.
    REPLACE ALL OCCURRENCES OF '>=' IN lv_xml WITH 'get'.


    out->write( lv_xml ).

  ENDMETHOD.
ENDCLASS.
