program main
  use, intrinsic :: iso_fortran_env
  use mathmodule
  use utilities

  INTEGER(INT8):: int8result
  INTEGER(INT16):: int16result
  INTEGER(INT32):: int32result
  INTEGER(INT64):: int64result

  REAL(REAL32):: real32result
  REAL(REAL64):: real64result
  REAL(REAL128):: real128result

  INTEGER(INT8):: int8var1,int8var2,int8resultVector
  INTEGER(INT16):: int16var1,int16var2,int16resultVector
  INTEGER(INT32):: int32var1,int32var2,int32resultVector
  INTEGER(INT64):: int64var1,int64var2,int64resultVector

  REAL(REAL32):: real32var1,real32var2,real32resultVector
  REAL(REAL64):: real64var1,real64var2,real64resultVector
  REAL(REAL128):: real128var1,real128var2,real128resultVector

  LOGICAL :: OperationSuccessful ,PrintSuccessful

  equivalence (int8resultVector,int8result)
  equivalence (int16resultVector,int16result)
  equivalence (int32resultVector,int32result)
  equivalence (int64resultVector,int64result)
  equivalence (real32resultVector,real32result)
  equivalence (real64resultVector,real64result)
  equivalence (real128resultVector,real128result)

  int8var1 = 50
  int8var2 = 30

  int16var1 = 32000
  int16var2 = 500
  int32var1 = 21473212
  int32var2 = 214733
  int64var1 = 1234567890
  int64var2 = 1222338

  real32var1 = 1.402823E+38
  real32var2 = 0.402823E+37
  real64var1 = 1.797693D+300
  real64var2 = 0.707003D+19
  real128var1 = 0.797693Q+208
  real128var2 = 0.000003Q+30

  OperationSuccessful = Add(int8var1,int8var2,int8resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 1
  PrintSuccessful = PrintResult(OperationSuccessful, int8result)

  OperationSuccessful = Add(int16var1,int16var2,int16resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 2
  PrintSuccessful = PrintResult(OperationSuccessful, int16result)

  OperationSuccessful = Add(int32var1,int32var2,int32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 3
  PrintSuccessful = PrintResult(OperationSuccessful, int32result)

  OperationSuccessful = Add(int64var1,int64var2,int64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 4
  PrintSuccessful = PrintResult(OperationSuccessful, int64result)

  OperationSuccessful = Add(real32var1,real32var2,real32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 5
  PrintSuccessful = PrintResult(OperationSuccessful, real32result)

  OperationSuccessful = Add(real64var1,real64var2,real64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 6
  PrintSuccessful = PrintResult(OperationSuccessful, real64result)

  OperationSuccessful = Add(real128var1,real128var2,real128resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 7
  PrintSuccessful = PrintResult(OperationSuccessful, real128result)

  OperationSuccessful = Mul(int8var1,int8var2,int8resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 8
  PrintSuccessful = PrintResult(OperationSuccessful, int8result)

  OperationSuccessful = Mul(int16var1,int16var2,int16resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 9
  PrintSuccessful = PrintResult(OperationSuccessful, int16result)

  OperationSuccessful = Mul(int32var1,int32var2,int32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 10
  PrintSuccessful = PrintResult(OperationSuccessful, int32result)

  OperationSuccessful = Mul(int64var1,int64var2,int64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 11
  PrintSuccessful = PrintResult(OperationSuccessful, int64result)

  OperationSuccessful = Mul(real32var1,real32var2,real32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 12
  PrintSuccessful = PrintResult(OperationSuccessful, real32result)

  OperationSuccessful = Mul(real64var1,real64var2,real64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 13
  PrintSuccessful = PrintResult(OperationSuccessful, real64result)

  OperationSuccessful = Mul(real128var1,real128var2,real128resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 14
  PrintSuccessful = PrintResult(OperationSuccessful, real128result)

  OperationSuccessful = Sub(int8var1,int8var2,int8resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 15
  PrintSuccessful = PrintResult(OperationSuccessful, int8result)

  OperationSuccessful = Sub(int16var1,int16var2,int16resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 16
  PrintSuccessful = PrintResult(OperationSuccessful, int16result)

  OperationSuccessful = Sub(int32var1,int32var2,int32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 17
  PrintSuccessful = PrintResult(OperationSuccessful, int32result)

  OperationSuccessful = Sub(int64var1,int64var2,int64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 18
  PrintSuccessful = PrintResult(OperationSuccessful, int64result)

  OperationSuccessful = Sub(real32var1,real32var2,real32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 19
  PrintSuccessful = PrintResult(OperationSuccessful, real32result)

  OperationSuccessful = Sub(real64var1,real64var2,real64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 20
  PrintSuccessful = PrintResult(OperationSuccessful, real64result)

  OperationSuccessful = Sub(real128var1,real128var2,real128resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 21
  PrintSuccessful = PrintResult(OperationSuccessful, real128result)

  OperationSuccessful = Div(int8var1,int8var2,int8resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 22
  PrintSuccessful = PrintResult(OperationSuccessful, int8result)

  OperationSuccessful = Div(int16var1,int16var2,int16resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 23
  PrintSuccessful = PrintResult(OperationSuccessful, int16result)

  OperationSuccessful = Div(int32var1,int32var2,int32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 24
  PrintSuccessful = PrintResult(OperationSuccessful, int32result)

  OperationSuccessful = Div(int64var1,int64var2,int64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 25
  PrintSuccessful = PrintResult(OperationSuccessful, int64result)

  OperationSuccessful = Div(real32var1,real32var2,real32resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 26
  PrintSuccessful = PrintResult(OperationSuccessful, real32result)

  OperationSuccessful = Div(real64var1,real64var2,real64resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 27
  PrintSuccessful = PrintResult(OperationSuccessful, real64result)

  OperationSuccessful = Div(real128var1,real128var2,real128resultVector)
  if (OperationSuccessful .NEQV. .true.) stop 28
  PrintSuccessful = PrintResult(OperationSuccessful, real128result)

end program
