namespace Fgu
  module DSL =
    open Fgu.Contracts
    open Fgu.Events

    let withLayers (contract: XLperRiskContract) layers = 
        {contract with layers = layers}

    let xs liability priority = Contracts.xs liability priority