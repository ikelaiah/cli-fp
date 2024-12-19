unit CLI.Errors;

{$mode objfpc}{$H+}{$J-}



interface

uses
Classes, SysUtils;

type
  ECLIException = class(Exception);
  ECommandNotFoundException = class(ECLIException);
  EInvalidParameterException = class(ECLIException);
  ERequiredParameterMissingException = class(ECLIException);
  EInvalidParameterValueException = class(ECLIException);
  ECommandExecutionException = class(ECLIException);

implementation

end.
