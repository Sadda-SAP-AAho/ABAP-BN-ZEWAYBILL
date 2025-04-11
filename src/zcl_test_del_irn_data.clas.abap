CLASS zcl_test_del_irn_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_test_del_irn_data IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.


    DELETE FROM ztable_irn WHERE billingdocno = '0090000650'.
    DELETE FROM ztable_irn WHERE billingdocno = '0090000355'.


  ENDMETHOD.
ENDCLASS.
