namespace Fgu
  module DSL =
    open Fgu.Contracts
    open Fgu.Events

    let withLayers (contract: XLperRiskContract) layers = 
        {contract with layers = layers}

    let xs liability priority = Contracts.xs liability priority

    // What makes the DSL: 
    //
    // quotaShare percentWe percentThem -> QuotaShare 
    // surPlus maximum ourFactor -> SurPlus
    //
    // xlPerRisk risks areas -> XLContract // mit applyPerRisk aber ohne layers
    // xlPerEvent risks areas -> XLContract // mit applyPerEvent aber ohne layers
    // stopLoss risk areas -> XLContract // mit applySL
    // 
    // withLayers XLContract -> Layer list -> XLContract 
    // withClasses XLContract -> Risk list -> XLContract  // class = Sparte (Feuer, Terror, Unfall, ...)
    // withAreas XLContract -> Area list -> XLContract
    //
    // MISSING:
    // * start and end date as a filter selection of events
    // * Dependencies between contracts
    // * any incomes for the re-insurance as an input for profit testing

    /// A simple definition of a quotaShare: without commision and without differentiating between
    /// our share of the overall reinsured contract. 
    let quotaShare (percentWe : Percent) : Contract = 
        QuotaShare { commission = 0.0; quota_share = percentWe; quota_part = 1.0 }


    /// A Surplus contract operates on the insured values, which occur in the FGU catalogs.
    ///
    let surplus maximum ourFactor = 
        SurPlus (makeSurPlus maximum ourFactor)

    /// Creates an XL per (NetCat) Risk contract. You have to add layers withLayers 
    let xlPerNatCatRisk risks =  makeXLperRisk risks []
    
    /// What is the difference between an XL per Risk and a XL per Event? 