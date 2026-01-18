@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS'
@Metadata.ignorePropagatedAnnotations: true
define root view entity Z_I_COUNTRY_COORD as select from zcountry_coord
{
    key country_iso as CountryIso,
    latitude as Latitude,
    longitude as Longitude,
    created_by as CreatedBy
}
