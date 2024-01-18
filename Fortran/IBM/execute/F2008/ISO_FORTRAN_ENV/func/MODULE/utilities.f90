module utilities
 use,intrinsic :: iso_fortran_env

 interface PrintResult
      module procedure Print_int8, &
                        Print_int16, &
                        Print_int32, &
                        Print_int64, &
                        Print_real32, &
                        Print_real64, &
                        Print_real128
 end interface

 contains

logical function Print_int8( x , y)
  implicit none

  logical, INTENT(IN) :: x
  INTEGER(INT8), INTENT(IN) :: y

  print*,x
  print*,y

  Print_int8= .true.

end function

logical function Print_int16( x , y)
  implicit none

  logical, INTENT(IN) :: x
  INTEGER(INT16), INTENT(IN) :: y

  print*,x
  print*,y

  Print_int16= .true.
end function

logical function Print_int32( x , y)
  implicit none

  logical, INTENT(IN) :: x
  INTEGER(INT32), INTENT(IN) :: y

  print*,x
  print*,y

  Print_int32 = .true.
end function

logical function Print_int64( x , y)
  implicit none

  logical, INTENT(IN) :: x
  INTEGER(INT64), INTENT(IN) :: y

  print*,x
  print*,y

  Print_int64 = .true.
end function

logical function Print_real32( x , y)
  implicit none

  logical, INTENT(IN) :: x
  REAL(REAL32), INTENT(IN) :: y

  print*,x
  print*,y

  Print_real32= .true.
end function

logical function Print_real64( x , y)
  implicit none

  logical, INTENT(IN) :: x
  REAL(REAL64), INTENT(IN) :: y

  print*,x
  print*,y

  Print_real64 = .true.
end function

logical function Print_real128( x , y)
  implicit none

  logical, INTENT(IN) :: x
  REAL(REAL128), INTENT(IN) :: y

  print*,x
  print*,y

  Print_real128 = .true.
end function

end module

