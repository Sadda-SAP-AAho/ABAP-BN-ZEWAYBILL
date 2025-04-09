CLASS zcl_ewaybillbyirn_generation DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ty_expshipdtls,
           addr1 TYPE string,
           addr2 TYPE string,
           loc   TYPE string,
           pin   TYPE string,
           stcd  TYPE string,
       END OF ty_expshipdtls.

    TYPES: BEGIN OF ty_dispdtls,
               nm    TYPE string,
               addr1 TYPE string,
               addr2 TYPE string,
               loc   TYPE string,
               pin   TYPE string,
               stcd  TYPE string,
           END OF ty_dispdtls.

    TYPES: BEGIN OF ty_json_data,
               irn          TYPE string,
               distance     TYPE i,
               transmode    TYPE string,
               transid      TYPE string,
               transname    TYPE string,
               transdocdt   TYPE string,
               transdocno   TYPE string,
               vehno        TYPE string,
               vehtype      TYPE string,
               expshipdtls  TYPE ty_expshipdtls,
               dispdtls     TYPE ty_dispdtls,
       END OF ty_json_data.

    CLASS-DATA: wa_final TYPE ty_json_data.
    CLASS-METHODS :generated_ewaybillbyirn IMPORTING
                                                     companycode   TYPE ztable_irn-bukrs
                                                     document      TYPE ztable_irn-billingdocno
                                           RETURNING VALUE(result) TYPE string.
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_EWAYBILLBYIRN_GENERATION IMPLEMENTATION.


  METHOD generated_ewaybillbyirn.


    SELECT SINGLE FROM i_billingdocument AS a
    INNER JOIN I_BillingDocumentItem AS b ON a~BillingDocument = b~BillingDocument
    FIELDS a~BillingDocument,
    a~BillingDocumentType,
    a~BillingDocumentDate,
    b~Plant,b~CompanyCode
    WHERE a~BillingDocument = @document
    INTO @DATA(lv_document_details) PRIVILEGED ACCESS.

     SELECT SINGLE FROM ztable_plant
    fields gstin_no, city, address1, address2, pin, state_code1,plant_name1, state_name
    WHERE plant_code = @lv_document_details-plant and comp_code = @lv_document_details-CompanyCode INTO @DATA(sellerplantaddress) PRIVILEGED ACCESS.

    wa_final-dispdtls-nm    =  sellerplantaddress-plant_name1.
    wa_final-dispdtls-addr1    =  sellerplantaddress-address1.
    wa_final-dispdtls-addr2    =  sellerplantaddress-address2.
    wa_final-dispdtls-loc      =  sellerplantaddress-address2 .
    IF sellerplantaddress-city IS NOT INITIAL.
      wa_final-dispdtls-loc      =  sellerplantaddress-city .
    ENDIF.
    wa_final-dispdtls-stcd     =  sellerplantaddress-state_code1.
    wa_final-dispdtls-pin      =  sellerplantaddress-pin.

    select single from zr_zirntp
        FIELDS Transportername, Vehiclenum, Grdate, Grno, Transportergstin, Irnno, Distance
        where Billingdocno = @document and Bukrs = @companycode
        INTO @DATA(Eway).

          if Eway-Irnno = ''.
            result = '1'.
            return.
          ENDIF.

        wa_final-vehno = Eway-Vehiclenum .
        wa_final-irn = Eway-Irnno .
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



    DATA:json TYPE REF TO if_xco_cp_json_data.

    xco_cp_json=>data->from_abap(
      EXPORTING
        ia_abap      = wa_final
      RECEIVING
        ro_json_data = json   ).
    json->to_string(
      RECEIVING
        rv_string =   DATA(lv_string) ).


    REPLACE ALL OCCURRENCES OF '"IRN"' IN lv_string WITH '"Irn"'.
    REPLACE ALL OCCURRENCES OF '"DISTANCE"' IN lv_string WITH '"Distance"'.
    REPLACE ALL OCCURRENCES OF '"TRANSMODE"' IN lv_string WITH '"TransMode"'.
    REPLACE ALL OCCURRENCES OF '"TRANSID"' IN lv_string WITH '"TransId"'.
    REPLACE ALL OCCURRENCES OF '"TRANSNAME"' IN lv_string WITH '"TransName"'.
    REPLACE ALL OCCURRENCES OF '"TRANSDOCDT"' IN lv_string WITH '"TransDocDt"'.
    REPLACE ALL OCCURRENCES OF '"TRANSDOCNO"' IN lv_string WITH '"TransDocNo"'.
    REPLACE ALL OCCURRENCES OF '"VEHNO"' IN lv_string WITH '"VehNo"'.
    REPLACE ALL OCCURRENCES OF '"VEHTYPE"' IN lv_string WITH '"VehType"'.
    REPLACE ALL OCCURRENCES OF '"EXPSHIPDTLS"' IN lv_string WITH '"ExpShipDtls"'.
    REPLACE ALL OCCURRENCES OF '"ADDR1"' IN lv_string WITH '"Addr1"'.
    REPLACE ALL OCCURRENCES OF '"ADDR2"' IN lv_string WITH '"Addr2"'.
    REPLACE ALL OCCURRENCES OF '"LOC"' IN lv_string WITH '"Loc"'.
    REPLACE ALL OCCURRENCES OF '"PIN"' IN lv_string WITH '"Pin"'.
    REPLACE ALL OCCURRENCES OF '"STCD"' IN lv_string WITH '"Stcd"'.
    REPLACE ALL OCCURRENCES OF '"DISPDTLS"' IN lv_string WITH '"DispDtls"'.
    REPLACE ALL OCCURRENCES OF '"NM"' IN lv_string WITH '"Nm"'.
    REPLACE ALL OCCURRENCES OF '"ExpShipDtls":{"Addr1":"","Addr2":"","Loc":"","Pin":"","Stcd":""}'
        IN lv_string WITH '"ExpShipDtls": null'.

    result = |[{ lv_string }]|.

  ENDMETHOD.
ENDCLASS.
