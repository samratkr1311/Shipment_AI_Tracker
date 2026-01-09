@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_SHIP_TRACKEVENT
  as projection on ZI_SHIP_TRACKEVENT
{

  key EventId,
      ShipmentId,
      EventType,
      EventLocation,
      EventTime,
      Remarks,
      /* Associations */
      _Shipment : redirected to parent ZC_SHIPMENT_TRACK
}
