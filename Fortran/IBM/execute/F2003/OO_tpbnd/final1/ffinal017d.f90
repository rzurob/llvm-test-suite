!**********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod 
! %COMPOPTS:  -qfree=f90
! %GROUP: ffinal017d.f 
! %VERIFY: ffinal017d.out:ffinal017d.vf
! %STDIN:
! %STDOUT: ffinal017d.out 
! %EXECARGS:
! %POSTCMD: 
! %END
!**********************************************************************
!**********************************************************************
!*  ===================================================================
!*  AIX XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : ffinal017d.f
!*  TEST CASE TITLE            : type-bound procedure
!*
!*  PROGRAMMER                 : Catherine Sun
!*  DATE                       : 
!*  ORIGIN                     : IBM Software Solutions Toronto Lab
!* 
!*  PRIMARY FUNCTIONS TESTED   : final subroutines 
!*
!*  SECONDARY FUNCTIONS TESTED : 
!*
!*  DESCRIPTION                : testing final subroutines: final
!*                               subroutines are not inherited  
!*                               through type extension.
!*    
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m
    type base
       integer :: x
    contains
       final :: finalizeBase
    end type
    
    type dt
       type(base), pointer :: dt_p => null()
    contains
       final :: finalize 
    end type

    contains
    subroutine finalizeBase (b1)
        type (base), intent(inout) :: b1
        print *, 'finalizeBase'
    end subroutine

    subroutine finalize (d1)
        type (dt), intent(inout) :: d1
        if(associated(d1%dt_p))    deallocate(d1%dt_p) 
    end subroutine
end module

   use m 

   call sub()

end

subroutine sub()
    use m
    type(dt) :: dt_c 
    allocate(dt_c%dt_p)
end subroutine
