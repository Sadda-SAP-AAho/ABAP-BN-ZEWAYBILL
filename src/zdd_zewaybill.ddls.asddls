@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Billing Lines CDS View'
define root view entity ZDD_ZEWAYBILL as select from I_BillingDocument
{
    key BillingDocument,
        SDDocumentCategory,
        BillingDocumentCategory,
        BillingDocumentType,
        CreatedByUser,
        CreationDate
}
