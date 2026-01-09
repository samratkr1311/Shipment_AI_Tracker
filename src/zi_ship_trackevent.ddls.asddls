@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_SHIP_TRACKEVENT
  as select from zship_trackevent
  association to parent ZI_SHIPMENT_TRACK as _Shipment on $projection.ShipmentId = _Shipment.ShipmentId

{
  key event_id       as EventId,
      shipment_id    as ShipmentId,
      event_type     as EventType,
      event_location as EventLocation,
      event_time     as EventTime,
      remarks        as Remarks,
      _Shipment
}
