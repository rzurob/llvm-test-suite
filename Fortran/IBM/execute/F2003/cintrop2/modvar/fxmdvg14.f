!#######################################################################
!************************************************************************
! %START
! %MAIN: YES
! %PRECMD:  $TR_SRC/run.sh fxmdvg14 cxmdvg14
! %COMPOPTS:   -qmixed
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
!************************************************************************
!**********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxmdvg14.f
!*
!*  DATE                       : Sep 14,2002
!*
!*  PRIMARY FUNCTIONS TESTED   :test integer(c_int_least32_t) with bind(c)
!*                              attribute/statement
!*                              test the effect on the binding lables
!*                              with -qmixed option.
!*  SECONDARY FUNCTIONS TESTED : None
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  DESCRIPTION                :
!*                              Pass data between a C variable with external
!*				linkage and Fortran variable has the bind(c)
!*				attribute.
!*                              Verify the result of data passing.
!*
!* ===================================================================
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!*  09/14/03    KT     -Initial Version
!* ===================================================================
!*
!234567890123456789012345678901234567890123456789012345678901234567890


module mod
  use iso_c_binding

  integer(c_int_least32_t), bind(c,name="xaa") :: fx=1

  integer(c_int_least32_t):: fy =2
  bind(c,name="Xaa") :: fy

  interface assert_eq2                   ! generic name
    module procedure assert2
  end interface

  contains

    function assert2(n1,n2)
      integer(c_int_least32_t), intent(in) :: n1,n2
      logical assert2
      if (n1 .ne. n2) then
      assert2= .false.
      else
      assert2=.true.
      endif
      return
    end function assert2

end module

program testmix
  use mod
  logical ::result
  integer(c_int_least32_t)::basval1,basval2

  basval1 =11
  basval2 =200
  print *, "variables fx and fy are initialized in fortran: fx = ",fx, "fy = ", fy
  call csub()
  result= assert_eq2(fx,basval1)

  if (result .eqv. .false.) then
  error stop 220
  endif


  result= (assert_eq2(fy,basval2))

  if (result .eqv. .false.) then
  error stop 221
  endif
end

