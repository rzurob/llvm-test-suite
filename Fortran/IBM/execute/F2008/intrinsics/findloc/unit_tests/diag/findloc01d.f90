!*********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2013-05-28
!*  ORIGIN                     :
!*
!*  PRIMARY FUNCTIONS TESTED   : findloc trinsic
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FINDLOC (ARRAY, VALUE, DIM [, MASK, KIND, BACK])
!*                               FINDLOC (ARRAY, VALUE [, MASK, KIND, BACK])
!*
!*  Test the compilation fails if:
!*
!*  * first argument should not be any type other than an array of integer,
!*    real, character, complex, logical and byte.
!*  * second argument should be scaler and in type conformance with first one.
!*  * third argument should  be an integer scalar with a value in the range of
!*    1 snd rank of ARRAY.
!*  * forth argument should be of type logical and shall be conformable with ARRAY.
!*  * fifth argument should be scalar integer constant expression.
!*  * sixth argument should be a logical scalar.
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================

PROGRAM findloc_diag

 integer loop_i, k1 = 4

 integer  :: array_i0 = 4, value_i = 2
 integer, dimension(2) :: array_i1 = (/2,4/)

 real  :: array_r0 = 4.0, value_r = 2.0
 real, dimension(4) :: array_r1 = (/1.0,2.0,3.0,4.0/)

 character(2)  :: array_ch0 = "ab", value_ch = "ab"
 character(2), dimension(2) :: array_ch1 = (/"ab", "cd"/)

 complex  :: array_co0 = (1,0), value_co = (1,0)
 complex, dimension(2) :: array_co1 = (/(1,0),(2,0)/)

 logical  :: array_log0 = .TRUE., value_log = .TRUE.
 logical, dimension(2) :: array_log1 = (/.FALSE.,.TRUE./)

 byte  :: array_b0 = 4, value_b = 2
 byte, dimension(2) :: array_b1 = (/2,4/)

 logical :: mask_1(1) = (/.TRUE./)

 !-- defining a derive type (array) to check that findloc does not accept it --!
 type TypeA
  integer :: i
  character(1) :: ch
 end type TypeA

 type (TypeA) :: TypeA_array(5), TypeA_value

 do loop_i = 1,5
  TypeA_array(loop_i)%i = loop_i
  TypeA_array(loop_i)%ch = char(loop_i+ichar("A")-1)
 end do

 TypeA_value%i = 1
 TypeA_value%ch = "A"

!--  first argument should not be any type other than an array of integer,real,
!--  complex, character, byte, logical:
print *, findloc(TypeA_array, TypeA_value, DIM=1)

!-- first argument can not be a acaler:
print *, findloc(array_i0, value_i)
print *, findloc(array_r0, value_r)
print *, findloc(array_ch0, value_ch)
print *, findloc(array_co0, value_co)
print *, findloc(array_log0, value_log)
print *, findloc(array_b0, value_b)

!-- second argument should be in type conformance with first one:
print *, findloc(array_i1, value_r)
print *, findloc(array_r1, value_ch)
print *, findloc(array_ch1, value_i)
print *, findloc(array_co1, value_i)
print *, findloc(array_log1, value_i)
print *, findloc(array_b1, value_r)

!-- third argument should be an integer scaler with value greater than 1 less than rank of ARRAY:
print *, findloc(array_i1, value_i, DIM = 1.0)
print *, findloc(array_i1, value_i, DIM = 2)
print *, findloc(array_i1, value_i, DIM = -1)

!-- forth argument should be of type logical and in type conformance with ARRAY:
print *, findloc(array_i1, value_i, 1, mask_1)
print *, findloc(array_r1, value_r, 1, mask_1)
print *, findloc(array_ch1, value_ch, 1, mask_1)
print *, findloc(array_co1, value_co, 1, mask_1)
print *, findloc(array_log1, value_log, 1, mask_1)
print *, findloc(array_b1, value_b, 1, mask_1)

print *, findloc(array_i1, value_i, 1, (/100,200/))

!-- fifth argument should be a scaler integer constant expression --!
print *, findloc(array_i1, value_i, 1, KIND = k1)
print *, findloc(array_i1, value_i, 1, KIND = 3)
print *, findloc(array_i1, value_i, 1, KIND = (/2/))

print *, findloc(array_r1, value_r, 1, KIND = k1)
print *, findloc(array_r1, value_r, 1, KIND = 16)
print *, findloc(array_r1, value_r, 1, KIND = (/2/))

print *, findloc(array_ch1, value_ch, 1, KIND = k1)
print *, findloc(array_ch1, value_ch, 1, KIND = 16)
print *, findloc(array_ch1, value_ch, 1, KIND = (/2/))

print *, findloc(array_co1, value_co, 1, KIND = k1)
print *, findloc(array_co1, value_co, 1, KIND = 16)
print *, findloc(array_co1, value_co, 1, KIND = (/2/))

print *, findloc(array_log1, value_log, 1, KIND = k1)
print *, findloc(array_log1, value_log, 1, KIND = 16)
print *, findloc(array_log1, value_log, 1, KIND = (/2/))

print *, findloc(array_b1, value_b, 1, KIND = k1)
print *, findloc(array_b1, value_b, 1, KIND = 16)
print *, findloc(array_b1, value_b, 1, KIND = (/2/)

!-- sixth argument should be a logical scaler --!
print *, findloc(array_i1, value_i, 1, BACK = 1)
print *, findloc(array_i1, value_i, 1, BACK = (/.TRUE./))

print *, findloc(array_r1, value_r, 1, BACK = 1)
print *, findloc(array_r1, value_r, 1, BACK = (/.TRUE./))

print *, findloc(array_co1, value_co, 1, BACK = 1)
print *, findloc(array_co1, value_co, 1, BACK = (/.TRUE./))

print *, findloc(array_log1, value_log, 1, BACK = 1)
print *, findloc(array_log1, value_log, 1, BACK = (/.TRUE./))

print *, findloc(array_b1, value_b, 1, BACK = 1)
print *, findloc(array_b1, value_b, 1, BACK = (/.TRUE./))

print *, findloc(array_ch1, value_ch, 1, BACK = 1)
print *, findloc(array_ch1, value_ch, 1, BACK = (/.TRUE./))

END

