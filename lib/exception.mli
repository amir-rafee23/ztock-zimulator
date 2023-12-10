(** [Exception] defines custom exceptions that are thrown by [Api] and
    [Portfolio] functions to be caught by [Tui] functions*)

exception NoResultsFound
exception InvalidJSONFormat
exception NoDateFound
exception NoClosingPriceFound
exception ExceededQuantity
exception TickerNotHeld
