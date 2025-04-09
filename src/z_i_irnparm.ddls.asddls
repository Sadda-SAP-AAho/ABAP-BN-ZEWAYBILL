@EndUserText.label: 'IRN Parameter'
define abstract entity Z_I_IRNPARM
{
  @EndUserText.label: 'Plant'
//  @Consumption.valueHelpDefinition: [{ entity: { name: 'I_PlantStdVH', element: 'Plant' } }]
  PlantNo     : werks_d;
 
  @EndUserText.label: 'Plan Date'
  PlanDate     : abap.dats;
    
}
