@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'CDS View'
@Metadata.ignorePropagatedAnnotations: true
define view entity ZI_PREDCTIONLOG
  as select from zai_predctionlog

  association to parent ZI_SHIPMENT_TRACK as _Prediction on $projection.ShipmentId = _Prediction.ShipmentId
{
  key request_id     as RequestId,
      shipment_id    as ShipmentId,
      ai_action_type as AiActionType,
      input_snapshot as InputSnapshot,
      ai_response    as AiResponse,
      called_at      as CalledAt,
      _Prediction
}
