module mathmodule
 use,intrinsic :: iso_fortran_env

 interface Add
     module procedure Add_int8, &
                        Add_int16, &
                        Add_int32, &
                        Add_int64, &
                        Add_real32, &
                        Add_real64, &
                        Add_real128
 end interface

 interface Div
     module procedure Div_int8, &
                        Div_int16, &
                        Div_int32, &
                        Div_int64, &
                        Div_real32, &
                        Div_real64, &
                        Div_real128
 end interface

 interface Sub
     module procedure Sub_int8, &
                        Sub_int16, &
                        Sub_int32, &
                        Sub_int64, &
                        Sub_real32, &
                        Sub_real64, &
                        Sub_real128
 end interface

 interface Mul
     module procedure Mul_int8, &
                        Mul_int16, &
                        Mul_int32, &
                        Mul_int64, &
                        Mul_real32, &
                        Mul_real64, &
                        Mul_real128
 end interface

 contains

logical function Add_int8( x , y, z)
  implicit none
  INTEGER(INT8), INTENT(IN) :: x,y
  INTEGER(INT8), INTENT(OUT) :: z

  z=x+y

  Add_int8=.true.
end function

logical function Add_int16( x , y, z)
  implicit none
  INTEGER(INT16), INTENT(IN) :: x,y
  INTEGER(INT16), INTENT(OUT) :: z

  z=x+y

  Add_int16=.true.
end function

logical function Add_int32( x , y, z)
  implicit none

  INTEGER(INT32), INTENT(IN) :: x,y
  INTEGER(INT32), INTENT(OUT) :: z

  z=x+y

  Add_int32 = .true.
end function

logical function Add_int64( x , y, z)
  implicit none

  INTEGER(INT64), INTENT(IN) :: x,y
  INTEGER(INT64), INTENT(OUT) :: z

  z=x+y

  Add_int64 = .true.
end function

logical function Add_real32( x , y, z)

  implicit none
  REAL(REAL32), INTENT(IN) :: x,y
  REAL(REAL32), INTENT(OUT) :: z
  z=x+y

  Add_real32 = .true.
end function

logical function Add_real64( x , y, z)
  implicit none

  REAL(REAL64), INTENT(IN) :: x,y
  REAL(REAL64), INTENT(OUT) :: z
  z=x+y

  Add_real64 = .true.
end function

logical function Add_real128( x , y, z)
  implicit none
  REAL(REAL128), INTENT(IN) :: x,y
  REAL(REAL128), INTENT(OUT) :: z

  z=x+y

  Add_real128 = .true.
end function

logical function Sub_int8( x , y, z)
  implicit none
  INTEGER(INT8), INTENT(IN) :: x,y
  INTEGER(INT8), INTENT(OUT) :: z

  z=x-y

  Sub_int8=.true.
end function

logical function Sub_int16( x , y, z)
  implicit none
  INTEGER(INT16), INTENT(IN) :: x,y
  INTEGER(INT16), INTENT(OUT) :: z

  z=x-y

  Sub_int16=.true.
end function

logical function Sub_int32( x , y, z)
  implicit none

  INTEGER(INT32), INTENT(IN) :: x,y
  INTEGER(INT32), INTENT(OUT) :: z

  z=x-y

  Sub_int32 = .true.
end function

logical function Sub_int64( x , y, z)
  implicit none

  INTEGER(INT64), INTENT(IN) :: x,y
  INTEGER(INT64), INTENT(OUT) :: z

  z=x-y

  Sub_int64 = .true.
end function

logical function Sub_real32( x , y, z)

  implicit none
  REAL(REAL32), INTENT(IN) :: x,y
  REAL(REAL32), INTENT(OUT) :: z
  z=x-y

  Sub_real32 = .true.
end function

logical function Sub_real64( x , y, z)
  implicit none

  REAL(REAL64), INTENT(IN) :: x,y
  REAL(REAL64), INTENT(OUT) :: z
  z=x-y

  Sub_real64 = .true.
end function

logical function Sub_real128( x , y, z)
  implicit none
  REAL(REAL128), INTENT(IN) :: x,y
  REAL(REAL128), INTENT(OUT) :: z

  z=x-y

  Sub_real128 = .true.
end function

logical function Mul_int8( x , y, z)
  implicit none
  INTEGER(INT8), INTENT(IN) :: x,y
  INTEGER(INT8), INTENT(OUT) :: z

  z=x*y

  Mul_int8=.true.
end function

logical function Mul_int16( x , y, z)
  implicit none
  INTEGER(INT16), INTENT(IN) :: x,y
  INTEGER(INT16), INTENT(OUT) :: z

  z=x*y

  Mul_int16=.true.
end function

logical function Mul_int32( x , y, z)
  implicit none

  INTEGER(INT32), INTENT(IN) :: x,y
  INTEGER(INT32), INTENT(OUT) :: z

  z=x*y

  Mul_int32 = .true.
end function

logical function Mul_int64( x , y, z)
  implicit none

  INTEGER(INT64), INTENT(IN) :: x,y
  INTEGER(INT64), INTENT(OUT) :: z

  z=x*y

  Mul_int64 = .true.
end function

logical function Mul_real32( x , y, z)

  implicit none
  REAL(REAL32), INTENT(IN) :: x,y
  REAL(REAL32), INTENT(OUT) :: z
  z=x*y

  Mul_real32 = .true.
end function

logical function Mul_real64( x , y, z)
  implicit none

  REAL(REAL64), INTENT(IN) :: x,y
  REAL(REAL64), INTENT(OUT) :: z
  z=x*y

  Mul_real64 = .true.
end function

logical function Mul_real128( x , y, z)
  implicit none
  REAL(REAL128), INTENT(IN) :: x,y
  REAL(REAL128), INTENT(OUT) :: z

  z=x*y

  Mul_real128 = .true.
end function

logical function Div_int8( x , y, z)
  implicit none
  INTEGER(INT8), INTENT(IN) :: x,y
  INTEGER(INT8), INTENT(OUT) :: z

 if (y .ne. 0) then
    z=x/y
    Div_int8 = .true.
  else
    Div_int8 = .false.
  end if

  Div_int8=.true.
end function

logical function Div_int16( x , y, z)
  implicit none
  INTEGER(INT16), INTENT(IN) :: x,y
  INTEGER(INT16), INTENT(OUT) :: z

  if (y .ne. 0) then
    z=x/y
    Div_int16 = .true.
  else
    Div_int16 = .false.
  end if

end function

logical function Div_int32( x , y, z)
  implicit none

  INTEGER(INT32), INTENT(IN) :: x,y
  INTEGER(INT32), INTENT(OUT) :: z

  if (y .ne. 0) then
    z=x/y
    Div_int32 = .true.
  else
    Div_int32 = .false.
  end if

end function

logical function Div_int64( x , y, z)
  implicit none

  INTEGER(INT64), INTENT(IN) :: x,y
  INTEGER(INT64), INTENT(OUT) :: z

  if (y .ne. 0) then
    z=x/y
    Div_int64 = .true.
  else
    Div_int64 = .false.
  end if

end function

logical function Div_real32( x , y, z)

  implicit none
  REAL(REAL32), INTENT(IN) :: x,y
  REAL(REAL32), INTENT(OUT) :: z

  if (y .ne. 0) then
    z=x/y
    Div_real32 = .true.
  else
    Div_real32 = .false.
  end if

end function

logical function Div_real64( x , y, z)
  implicit none

  REAL(REAL64), INTENT(IN) :: x,y
  REAL(REAL64), INTENT(OUT) :: z

  if (y .ne. 0) then
    z=x/y
    Div_real64 = .true.
  else
    Div_real64 = .false.
  end if

end function

logical function Div_real128( x , y, z)
  implicit none
  REAL(REAL128), INTENT(IN) :: x,y
  REAL(REAL128), INTENT(OUT) :: z

  if (y .ne. 0) then
    z=x/y
    Div_real128 = .true.
  else
    Div_real128 = .false.
  end if

end function

end module

