@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Projection View'
@Metadata.ignorePropagatedAnnotations: true
@Metadata.allowExtensions: true
define view entity ZC_PREDCTIONLOG
  as projection on ZI_PREDCTIONLOG
{
  key RequestId,
      ShipmentId,
      AiActionType,
      InputSnapshot,
      AiResponse,
      CalledAt,
      /* Associations */
      _Prediction : redirected to parent ZC_SHIPMENT_TRACK
}
