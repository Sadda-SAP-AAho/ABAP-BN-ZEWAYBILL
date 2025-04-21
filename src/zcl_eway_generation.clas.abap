CLASS zcl_eway_generation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
TYPES: BEGIN OF ty_item_list,
           ProdName            TYPE string,
           ProdDesc            TYPE string,
           HsnCd               TYPE string,
           Qty                 TYPE P LENGTH 13 DECIMALS 3,
           Unit                TYPE string,
           AssAmt              TYPE P LENGTH 13 DECIMALS 3,
           CgstRt              TYPE P LENGTH 13 DECIMALS 3,
           CgstAmt             TYPE P LENGTH 13 DECIMALS 3,
           SgstRt              TYPE P LENGTH 13 DECIMALS 3,
           SgstAmt             TYPE P LENGTH 13 DECIMALS 3,
           IgstRt              TYPE P LENGTH 13 DECIMALS 3,
           IgstAmt             TYPE P LENGTH 13 DECIMALS 3,
           CesRt               TYPE P LENGTH 13 DECIMALS 3,
           CesAmt              TYPE P LENGTH 13 DECIMALS 3,
           OthChrg             TYPE P LENGTH 13 DECIMALS 3,
           CesNonAdvAmt        TYPE P LENGTH 13 DECIMALS 3,
       END OF ty_item_list.

     CLASS-DATA itemList TYPE TABLE OF ty_item_list.

TYPES: BEGIN OF ty_address_details,
           Gstin   TYPE string,
           LglNm   TYPE string,
           TrdNm   TYPE string,
           Addr1   TYPE string,
           Addr2   TYPE string,
           Loc     TYPE string,
           Pin     TYPE string,
           Stcd    TYPE string,
       END OF ty_address_details.

         CLASS-DATA SellerDtls TYPE  ty_address_details.
         CLASS-DATA BuyerDtls  TYPE  ty_address_details.

TYPES: BEGIN OF ty_exp_ship_dtls,
           Addr1 TYPE string,
           Addr2 TYPE string,
           Loc   TYPE string,
           Pin   TYPE string,
           Stcd  TYPE string,
       END OF ty_exp_ship_dtls.

      CLASS-DATA ExpShipDtls  TYPE  ty_exp_ship_dtls.

TYPES: BEGIN OF ty_disp_dtls,
           Nm    TYPE string,
           Addr1 TYPE string,
           Addr2 TYPE string,
           Loc   TYPE string,
           Pin   TYPE string,
           Stcd  TYPE string,
       END OF ty_disp_dtls.

      CLASS-DATA DispDtls  TYPE  ty_disp_dtls.

TYPES: BEGIN OF ty_main_document,
           DocumentNumber        TYPE string,
           DocumentType          TYPE string,
           DocumentDate          TYPE string,
           SupplyType            TYPE string,
           SubSupplyType         TYPE string,
           SubSupplyTypeDesc     TYPE string,
           TransactionType       TYPE string,
           BuyerDtls             TYPE ty_address_details,
           SellerDtls            TYPE ty_address_details,
           ExpShipDtls           TYPE ty_exp_ship_dtls,
           DispDtls              TYPE ty_disp_dtls,
           TotalInvoiceAmount    TYPE P LENGTH 13 DECIMALS 3,
           TotalCgstAmount       TYPE P LENGTH 13 DECIMALS 3,
           TotalSgstAmount       TYPE P LENGTH 13 DECIMALS 3,
           TotalIgstAmount       TYPE P LENGTH 13 DECIMALS 3,
           TotalCessAmount       TYPE P LENGTH 13 DECIMALS 3,
           TotalCessNonAdvolAmount TYPE P LENGTH 13 DECIMALS 3,
           TotalAssessableAmount TYPE P LENGTH 13 DECIMALS 3,
           OtherAmount           TYPE P LENGTH 13 DECIMALS 3,
           OtherTcsAmount        TYPE P LENGTH 13 DECIMALS 3,
           TransId               TYPE string,
           TransName             TYPE string,
           TransMode             TYPE string,
           Distance              TYPE P LENGTH 13 DECIMALS 3,
           TransDocNo            TYPE string,
           TransDocDt            TYPE string,
           VehNo                 TYPE string,
           VehType               TYPE string,
           ItemList              LIKE itemlist,
       END OF ty_main_document.

    CLASS-DATA: wa_final TYPE ty_main_document.

    CLASS-METHODS :generated_eway_bill IMPORTING
                                                 invoice       TYPE ztable_irn-billingdocno
                                                 companycode   TYPE ztable_irn-bukrs
                                       RETURNING VALUE(result) TYPE string.
protected section.
private section.
ENDCLASS.



CLASS ZCL_EWAY_GENERATION IMPLEMENTATION.


  METHOD generated_eway_bill.

  DATA :        wa_itemlist TYPE ty_item_list.

     SELECT SINGLE FROM i_billingdocument AS a
    INNER JOIN I_BillingDocumentItem AS b ON a~BillingDocument = b~BillingDocument
    FIELDS a~BillingDocument,
    a~BillingDocumentType,
    a~BillingDocumentDate,
    b~Plant,a~CompanyCode, a~DocumentReferenceID
    WHERE a~BillingDocument = @invoice
    INTO @DATA(lv_document_details) PRIVILEGED ACCESS.


    SHIFT lv_document_details-BillingDocument LEFT DELETING LEADING '0'.
    wa_final-documentnumber = lv_document_details-DocumentReferenceID.
    wa_final-documentdate = lv_document_details-BillingDocumentDate+6(2) && '/' && lv_document_details-BillingDocumentDate+4(2) && '/' && lv_document_details-BillingDocumentDate(4).


    wa_final-supplytype = 'OUTWARD'.
     IF lv_document_details-BillingDocumentType = 'JDC' OR lv_document_details-BillingDocumentType = 'JSN' OR lv_document_details-BillingDocumentType = 'JVR'
        OR lv_document_details-BillingDocumentType = 'JSP' OR lv_document_details-BillingDocumentType = 'JSTO'.
        wa_final-subsupplytype = '5'.
        wa_final-documenttype = 'CHL'.
    ELSE.
        wa_final-subsupplytype = '1'.
        wa_final-documenttype = 'INV'.
    ENDIF.


*   ***********************************seller detials

    SELECT SINGLE FROM ztable_plant
    fields gstin_no, city, address1, address2, pin, state_code1,plant_name1, state_name
    WHERE plant_code = @lv_document_details-plant and comp_code = @lv_document_details-CompanyCode INTO @DATA(sellerplantaddress) PRIVILEGED ACCESS.

    wa_final-sellerdtls-gstin    =  sellerplantaddress-gstin_no.
    wa_final-sellerdtls-lglnm  =  sellerplantaddress-plant_name1.
    wa_final-sellerdtls-trdnm =  sellerplantaddress-plant_name1.
    wa_final-sellerdtls-addr1    =  sellerplantaddress-address1.
    wa_final-sellerdtls-addr2    =  sellerplantaddress-address2 .
    wa_final-sellerdtls-loc      =  sellerplantaddress-address2 .
    IF sellerplantaddress-city IS NOT INITIAL.
      wa_final-sellerdtls-loc      =  sellerplantaddress-city .
    ENDIF.
    wa_final-sellerdtls-stcd     =  sellerplantaddress-state_code1.
    wa_final-sellerdtls-pin      =  sellerplantaddress-pin.


*******************************    buyer details

    SELECT SINGLE * FROM i_billingdocumentpartner AS a  INNER JOIN i_customer AS
            b ON ( a~customer = b~customer  ) WHERE a~billingdocument = @invoice
             AND a~partnerfunction = 'RE' INTO  @DATA(buyeradd) PRIVILEGED ACCESS.


    wa_final-buyerdtls-gstin = buyeradd-b-taxnumber3.
    wa_final-buyerdtls-lglnm = buyeradd-b-customername.
    wa_final-buyerdtls-trdnm = buyeradd-b-customername.
    wa_final-buyerdtls-addr1 = buyeradd-b-customerfullname.
    wa_final-buyerdtls-addr2 = ''.
    wa_final-buyerdtls-loc   = buyeradd-b-cityname .
    wa_final-buyerdtls-pin   = buyeradd-b-postalcode  .
    wa_final-buyerdtls-stcd  = buyeradd-b-TaxNumber3+0(2)  .

****************************    dispatch details


    wa_final-dispdtls-nm    =  sellerplantaddress-plant_name1.
    wa_final-dispdtls-addr1    =  sellerplantaddress-address1.
    wa_final-dispdtls-addr2    =  sellerplantaddress-address2.
    wa_final-dispdtls-loc      =  sellerplantaddress-address2 .
    IF sellerplantaddress-city IS NOT INITIAL.
      wa_final-dispdtls-loc      =  sellerplantaddress-city .
    ENDIF.
    wa_final-dispdtls-stcd     =  sellerplantaddress-state_code1.
    wa_final-dispdtls-pin      =  sellerplantaddress-pin.


    SELECT FROM I_BillingDocumentItem FIELDS BillingDocument, BillingDocumentItem, BillingDocumentItemText,
    Product, Plant, BillingQuantity, BillingQuantityUnit
    WHERE BillingDocument = @invoice AND CompanyCode = @companycode
    INTO TABLE @DATA(lt_item) PRIVILEGED ACCESS.




    select single from zr_zirntp
    FIELDS Transportername, Vehiclenum, Grdate, Grno, Transportergstin, Distance, Ewaytranstype
    where Billingdocno = @invoice and Bukrs = @companycode
    INTO @DATA(Eway).

    wa_final-vehno = Eway-Vehiclenum .
    wa_final-transname = Eway-Transportername .
    wa_final-transdocdt = Eway-Grdate+6(2) && '/' && Eway-Grdate+4(2) && '/' && Eway-Grdate(4).
    wa_final-transdocno = Eway-Grno .
    wa_final-transid = Eway-Transportergstin .
    wa_final-transmode = '1'.
    IF Eway-Distance = 0.
        wa_final-distance = 0.
    ELSE.
        wa_final-distance = Eway-Distance.
    ENDIF.
    wa_final-vehtype = 'R'.

    SELECT SINGLE FROM zr_ewaytranstype
    FIELDS Value
    WHERE Description = @Eway-Ewaytranstype
    INTO @wa_final-transactiontype.


    SELECT FROM I_BillingDocumentItem AS item
        LEFT JOIN I_ProductDescription AS pd ON item~Product = pd~Product AND pd~LanguageISOCode = 'EN'
        LEFT JOIN i_productplantbasic AS c ON item~Product = c~Product and item~Plant = c~Plant
        FIELDS item~BillingDocument, item~BillingDocumentItem
        , item~Plant, item~ProfitCenter, item~Product, item~BillingQuantity, item~BaseUnit, item~BillingQuantityUnit, item~NetAmount,
             item~TaxAmount, item~TransactionCurrency, item~CancelledBillingDocument, item~BillingQuantityinBaseUnit,
             pd~ProductDescription,
             c~consumptiontaxctrlcode
        WHERE item~BillingDocument = @invoice AND consumptiontaxctrlcode IS NOT INITIAL
           INTO TABLE @DATA(ltlines).

      SELECT FROM I_BillingDocItemPrcgElmntBasic FIELDS BillingDocument , BillingDocumentItem, ConditionRateValue, ConditionAmount, ConditionType,
        transactioncurrency AS d_transactioncurrency
        WHERE BillingDocument = @invoice
        INTO TABLE @DATA(it_price).

      LOOP AT ltlines INTO DATA(wa_lines).
        wa_itemlist-prodname = wa_lines-ProductDescription.
        wa_itemlist-proddesc = wa_lines-ProductDescription.
        wa_itemlist-hsncd = wa_lines-consumptiontaxctrlcode.
        wa_itemlist-qty = wa_lines-BillingQuantity.

        select single from zgstuom
            FIELDS gstuom
            where uom = @wa_lines-BillingQuantityUnit "and bukrs = @wa_lines-CompanyCode
            into @DATA(UOM).

        if UOM is INITIAL.
            wa_itemlist-unit = wa_lines-BillingQuantityUnit.
        ELSE.
            wa_itemlist-unit = UOM.
        ENDIF.


         READ TABLE it_price INTO DATA(wa_price1) WITH KEY BillingDocument = wa_lines-BillingDocument
                                                         BillingDocumentItem = wa_lines-BillingDocumentItem
                                                         ConditionType = 'JOIG'.
        if wa_price1 is not INITIAL.
            wa_itemlist-igstrt                       = wa_price1-ConditionRateValue.
            wa_itemlist-igstamt                       = wa_price1-ConditionAmount.
            CLEAR wa_price1.

        ELSE.

            READ TABLE it_price INTO DATA(wa_price2) WITH KEY BillingDocument = wa_lines-BillingDocument
                                                             BillingDocumentItem = wa_lines-BillingDocumentItem
                                                             ConditionType = 'JOSG'.
            wa_itemlist-sgstamt                    = wa_price2-ConditionAmount.
            wa_itemlist-sgstrt                    = wa_price2-ConditionRateValue.

             READ TABLE it_price INTO DATA(wa_price3) WITH KEY BillingDocument = wa_lines-BillingDocument
                                                             BillingDocumentItem = wa_lines-BillingDocumentItem
                                                             ConditionType = 'JOCG'.
            wa_itemlist-cgstrt                    = wa_price3-ConditionRateValue.
            wa_itemlist-cgstamt                    = wa_price3-ConditionAmount.

            CLEAR : wa_price2,wa_price3.
        ENDIF.


         READ TABLE it_price INTO DATA(tcs) WITH KEY  BillingDocument = wa_lines-BillingDocument
                                                             BillingDocumentItem = wa_lines-BillingDocumentItem
                                                             ConditionType = 'JTC1'.
          IF tcs is not INITIAL .
            DATA(tcsamt) = tcs-ConditionAmount .
          ENDIF.

          SELECT SUM( conditionamount )    FROM i_billingdocumentitemprcgelmnt
             WHERE   conditiontype IN (  'ZCD1', 'ZCD2','ZD01','ZD02','ZRAB','D100' )
             AND billingdocument = @wa_lines-billingdocument AND billingdocumentitem = @wa_lines-billingdocumentitem
              INTO @DATA(discount) .

            if discount < 0.
               discount                     =      discount * -1.
            ENDIF.

           SELECT SUM( conditionamount )    FROM i_billingdocumentitemprcgelmnt
             WHERE   conditiontype IN ( 'YBHD', 'ZHF1','ZIF1','ZBK1','ZHI1', 'FIN1' ,'ZHP1','ZCA1'  )
             AND billingdocument = @wa_lines-billingdocument AND billingdocumentitem = @wa_lines-billingdocumentitem
              INTO @DATA(OtherCharges) .


        SELECT   FROM i_billingdocumentitemprcgelmnt as a
        fields  SUM( a~ConditionRateAmount ) as UnitPrice, sum( a~ConditionAmount ) as TotAmt
         WHERE   conditiontype IN ( 'ZPR0','ZCIP' )
         AND billingdocument = @wa_lines-billingdocument AND billingdocumentitem = @wa_lines-billingdocumentitem
          INTO @DATA(unitprice) .

          wa_itemlist-othchrg = tcsamt.
          wa_itemlist-assamt = unitprice-TotAmt - discount + OtherCharges.


          wa_final-totalassessableamount   +=   + wa_itemlist-assamt.
          wa_final-totaligstamount +=  wa_itemlist-igstamt.
          wa_final-totalcgstamount +=  wa_itemlist-cgstamt.
          wa_final-totalsgstamount +=  wa_itemlist-sgstamt.
          wa_final-othertcsamount +=  tcsamt.

      APPEND wa_itemlist TO itemList.
      CLEAR :  wa_itemlist.
    ENDLOOP.

      wa_final-totalinvoiceamount = wa_final-totalassessableamount + wa_final-totaligstamount + wa_final-totalcgstamount + wa_final-totalsgstamount + wa_final-othertcsamount.
      wa_final-itemlist = itemList.

    DATA:json TYPE REF TO if_xco_cp_json_data.

    xco_cp_json=>data->from_abap(
      EXPORTING
        ia_abap      = wa_final
      RECEIVING
        ro_json_data = json   ).
    json->to_string(
      RECEIVING
        rv_string =   DATA(lv_string) ).

    REPLACE ALL OCCURRENCES OF '"DOCUMENTNUMBER"'       IN lv_string WITH '"DocumentNumber"'.
    REPLACE ALL OCCURRENCES OF '"DOCUMENTDATE"'         IN lv_string WITH '"DocumentDate"'.
    REPLACE ALL OCCURRENCES OF '"DOCUMENTTYPE"'         IN lv_string WITH '"DocumentType"'.
    REPLACE ALL OCCURRENCES OF '"SUPPLYTYPE"'           IN lv_string WITH '"SupplyType"'.
    REPLACE ALL OCCURRENCES OF '"SUBSUPPLYTYPE"'        IN lv_string WITH '"SubSupplyType"'.
    REPLACE ALL OCCURRENCES OF '"SUBSUPPLYTYPEDESC"'    IN lv_string WITH '"SubSupplyTypeDesc"'.
    REPLACE ALL OCCURRENCES OF '"TRANSACTIONTYPE"'      IN lv_string WITH '"TransactionType"'.

    REPLACE ALL OCCURRENCES OF '"BUYERDTLS"'            IN lv_string WITH '"BuyerDtls"'.
    REPLACE ALL OCCURRENCES OF '"SELLERDTLS"'           IN lv_string WITH '"SellerDtls"'.
    REPLACE ALL OCCURRENCES OF '"EXPSHIPDTLS"'          IN lv_string WITH '"ExpShipDtls"'.
    REPLACE ALL OCCURRENCES OF '"DISPDTLS"'             IN lv_string WITH '"DispDtls"'.

    REPLACE ALL OCCURRENCES OF '"ITEMLIST"'             IN lv_string WITH '"ItemList"'.
    REPLACE ALL OCCURRENCES OF '"PRODNAME"'             IN lv_string WITH '"ProdName"'.
    REPLACE ALL OCCURRENCES OF '"PRODDESC"'             IN lv_string WITH '"ProdDesc"'.
    REPLACE ALL OCCURRENCES OF '"HSNCD"'                IN lv_string WITH '"HsnCd"'.
    REPLACE ALL OCCURRENCES OF '"QTY"'                  IN lv_string WITH '"Qty"'.
    REPLACE ALL OCCURRENCES OF '"UNIT"'                 IN lv_string WITH '"Unit"'.
    REPLACE ALL OCCURRENCES OF '"ASSAMT"'               IN lv_string WITH '"AssAmt"'.
    REPLACE ALL OCCURRENCES OF '"CGSTRT"'               IN lv_string WITH '"CgstRt"'.
    REPLACE ALL OCCURRENCES OF '"CGSTAMT"'              IN lv_string WITH '"CgstAmt"'.
    REPLACE ALL OCCURRENCES OF '"SGSTRT"'               IN lv_string WITH '"SgstRt"'.
    REPLACE ALL OCCURRENCES OF '"SGSTAMT"'              IN lv_string WITH '"SgstAmt"'.
    REPLACE ALL OCCURRENCES OF '"IGSTRT"'               IN lv_string WITH '"IgstRt"'.
    REPLACE ALL OCCURRENCES OF '"IGSTAMT"'              IN lv_string WITH '"IgstAmt"'.
    REPLACE ALL OCCURRENCES OF '"CESRT"'                IN lv_string WITH '"CesRt"'.
    REPLACE ALL OCCURRENCES OF '"CESAMT"'               IN lv_string WITH '"CesAmt"'.
    REPLACE ALL OCCURRENCES OF '"OTHCHRG"'              IN lv_string WITH '"OthChrg"'.
    REPLACE ALL OCCURRENCES OF '"CESNONADVAMT"'         IN lv_string WITH '"CesNonAdvAmt"'.

    REPLACE ALL OCCURRENCES OF '"TOTALINVOICEAMOUNT"'   IN lv_string WITH '"TotalInvoiceAmount"'.
    REPLACE ALL OCCURRENCES OF '"TOTALCGSTAMOUNT"'      IN lv_string WITH '"TotalCgstAmount"'.
    REPLACE ALL OCCURRENCES OF '"TOTALSGSTAMOUNT"'      IN lv_string WITH '"TotalSgstAmount"'.
    REPLACE ALL OCCURRENCES OF '"TOTALIGSTAMOUNT"'      IN lv_string WITH '"TotalIgstAmount"'.
    REPLACE ALL OCCURRENCES OF '"TOTALCESSAMOUNT"'      IN lv_string WITH '"TotalCessAmount"'.
    REPLACE ALL OCCURRENCES OF '"TOTALCESSNONADVOLAMOUNT"' IN lv_string WITH '"TotalCessNonAdvolAmount"'.
    REPLACE ALL OCCURRENCES OF '"TOTALASSESSABLEAMOUNT"' IN lv_string WITH '"TotalAssessableAmount"'.
    REPLACE ALL OCCURRENCES OF '"OTHERAMOUNT"'          IN lv_string WITH '"OtherAmount"'.
    REPLACE ALL OCCURRENCES OF '"OTHERTCSAMOUNT"'       IN lv_string WITH '"OtherTcsAmount"'.

    REPLACE ALL OCCURRENCES OF '"TRANSID"'              IN lv_string WITH '"TransId"'.
    REPLACE ALL OCCURRENCES OF '"TRANSNAME"'            IN lv_string WITH '"TransName"'.
    REPLACE ALL OCCURRENCES OF '"TRANSMODE"'            IN lv_string WITH '"TransMode"'.
    REPLACE ALL OCCURRENCES OF '"DISTANCE"'             IN lv_string WITH '"Distance"'.
    REPLACE ALL OCCURRENCES OF '"TRANSDOCNO"'           IN lv_string WITH '"TransDocNo"'.
    REPLACE ALL OCCURRENCES OF '"TRANSDOCDT"'           IN lv_string WITH '"TransDocDt"'.
    REPLACE ALL OCCURRENCES OF '"VEHNO"'                IN lv_string WITH '"VehNo"'.
    REPLACE ALL OCCURRENCES OF '"VEHTYPE"'              IN lv_string WITH '"VehType"'.


    REPLACE ALL OCCURRENCES OF '"GSTIN"' IN lv_string WITH '"Gstin"'.
    REPLACE ALL OCCURRENCES OF '"TransId":""' IN lv_string WITH '"TransId":null'.
    REPLACE ALL OCCURRENCES OF '"TransName":""' IN lv_string WITH '"TransName":null'.
    REPLACE ALL OCCURRENCES OF '"TransDocNo":""' IN lv_string WITH '"TransDocNo":null'.
    REPLACE ALL OCCURRENCES OF '"LGLNM"' IN lv_string WITH '"LglNm"'.
    REPLACE ALL OCCURRENCES OF '"NM"' IN lv_string WITH '"Nm"'.
    REPLACE ALL OCCURRENCES OF '"TRDNM"' IN lv_string WITH '"TrdNm"'.
    REPLACE ALL OCCURRENCES OF '"ADDR1"' IN lv_string WITH '"Addr1"'.
    REPLACE ALL OCCURRENCES OF '"ADDR2"' IN lv_string WITH '"Addr2"'.
    REPLACE ALL OCCURRENCES OF '"LOC"' IN lv_string WITH '"Loc"'.
    REPLACE ALL OCCURRENCES OF '"PIN"' IN lv_string WITH '"Pin"'.
    REPLACE ALL OCCURRENCES OF '"STCD"' IN lv_string WITH '"Stcd"'.
    REPLACE ALL OCCURRENCES OF '"ExpShipDtls":{"Addr1":"","Addr2":"","Loc":"","Pin":"","Stcd":""}'
        IN lv_string WITH '"ExpShipDtls": null'.

    result = lv_string.

  ENDMETHOD.
ENDCLASS.
