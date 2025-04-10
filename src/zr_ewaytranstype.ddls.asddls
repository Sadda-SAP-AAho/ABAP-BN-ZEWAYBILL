@AbapCatalog.sqlViewName: 'ZEWAYTRANSTYPE'
@AbapCatalog.compiler.compareFilter: true
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Eway Trans Type CDS'
@Metadata.ignorePropagatedAnnotations: true
define view ZR_EWAYTRANSTYPE as select from DDCDS_CUSTOMER_DOMAIN_VALUE_T( p_domain_name:'ZEWAYTRANSTTYPE' )
{
    @EndUserText.label: 'Value'
    key value_low as Value,
    @Semantics.text: true
    text as Description
}
