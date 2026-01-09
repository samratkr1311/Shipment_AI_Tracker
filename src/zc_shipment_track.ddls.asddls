@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define root view entity ZC_SHIPMENT_TRACK
  as projection on ZI_SHIPMENT_TRACK
{
  key ShipmentId,
      ShipmentType,
//      ShpTDesc,
      OriginLocation,
      DestinationLocation,
      DistanceKm,
      CurrentStatus,
      EtaDays,
      DelayHours,
      DelayRisk,
      AiSummary,
      CreateBy,
      CreatedAt,
      UpdatedAt,
      _Shptype,
      _Country,
      /* Associations */
      _Prediction : redirected to composition child ZC_PREDCTIONLOG,
      _TrackerEvent : redirected to composition child ZC_SHIP_TRACKEVENT
}
