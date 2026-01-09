@AbapCatalog.viewEnhancementCategory: [#NONE]
@AccessControl.authorizationCheck: #NOT_REQUIRED
@EndUserText.label: 'Cds view for country'
@Metadata.ignorePropagatedAnnotations: true
@ObjectModel.usageType:{
    serviceQuality: #X,
    sizeCategory: #S,
    dataClass: #MIXED
}
define view entity ZI_COUNTRY as select from I_Country
{
    key Country as Country,
    CountryThreeLetterISOCode,
    CountryThreeDigitISOCode,
    CountryISOCode,
    IsEuropeanUnionMember,
    BankAndBankInternalIDCheckRule,
    BankInternalIDLength,
    BankInternalIDCheckRule,
    BankNumberLength,
    BankCheckRule,
    BankAccountLength,
    BankPostalCheckRule,
    BankDataCheckIsCountrySpecific,
    /* Associations */
    _Text
}
