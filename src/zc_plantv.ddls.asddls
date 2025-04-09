@AccessControl.authorizationCheck: #NOT_REQUIRED
@Metadata.allowExtensions: true
@Search.searchable: true
define root view entity zc_plantv
  as select from I_Plant
{
  key Plant,
      PlantName
}
