@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View'
@Metadata.ignorePropagatedAnnotations: true
define root view entity ZI_SHIPMENT_TRACK
  as select from zshipment_header
  composition [0..*] of ZI_SHIP_TRACKEVENT as _TrackerEvent

  composition [0..*] of ZI_PREDCTIONLOG    as _Prediction
  association to ZI_DOM_SHPT_TYPE          as _Shptype on $projection.ShipmentType = _Shptype.Description
  association to ZI_COUNTRY                as _Country on $projection.OriginLocation = _Country.Country
{

  key shipment_id          as ShipmentId,
      shipment_type        as ShipmentType,
      origin_location      as OriginLocation,
      destination_location as DestinationLocation,
      distance_km          as DistanceKm,
      current_status       as CurrentStatus,
      eta_days             as EtaDays,
      delay_hours          as DelayHours,
      delay_risk           as DelayRisk,
      delay_risk_crit      as DelayRiskCrit, 
      ai_summary           as AiSummary,
      create_by            as CreateBy,
      created_at           as CreatedAt,
      updated_at           as UpdatedAt,
      _TrackerEvent,
      _Prediction,
      _Shptype,
      _Country
      //      _Shptype.Description as ShpTDesc

}
