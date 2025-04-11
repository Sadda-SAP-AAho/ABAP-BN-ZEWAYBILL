@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View forZIRN'
define root view entity ZI_ZIRNTP
  provider contract transactional_interface
  as projection on ZR_ZIRNTP as ZIRN
{
  key Bukrs,
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
  Canceldate,
  IRNCancelDate,
  EwayValidDate,
  Signedinvoice,
  Signedqrcode,
  Ewaytranstype,
  Distance,
  Vehiclenum,
  Transportername,
  Transportergstin,
  Transportmode,
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
