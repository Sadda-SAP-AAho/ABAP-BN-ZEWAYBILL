@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@EndUserText.label: 'Projection View forZIRN'
@ObjectModel.semanticKey: [ 'Bukrs' ]
@Search.searchable: true
define root view entity ZC_ZIRNTP
  provider contract transactional_query
  as projection on ZR_ZIRNTP as ZIRN
{
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key Bukrs,
  @Search.defaultSearchElement: true
  @Search.fuzzinessThreshold: 0.90 
  key Billingdocno,
    //added 16-03-2025
  key GSTno,
  Moduletype,
  Plant,
  Billingdate,
  Partycode,
  distributionchannel,
  billingdocumenttype,
  Partyname,
  Irnno,
  Ackno,
  Ackdate,
  documentreferenceid,
  Irnstatus,
  IRNCancelDate,
  Canceldate,
  EwayValidDate,
  Distance,
  Vehiclenum,
  Ewaytranstype,
  Transportername,
  Transportergstin,
  Transportmode ,
  Grno,
  Grdate,
  Ewaybillno,
  Ewaydate,
  Ewaystatus,
  Ewaycanceldate,
  Irncreatedby,
  Ewaycreatedby,
  CreatedBy,
  CreatedAt,
  LastChangedBy,
  LastChangedAt,
  LocalLastChangedAt
  
}
