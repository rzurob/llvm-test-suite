!#######################################################################
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  PROGRAMMER                 : William Zhang 
!*  DATE                       : 30/05/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : pointer array, VOLATILE
!*
!*
!*  DESCRIPTION                : funtional TC for  C1233
!*
!*  C1233 (R1221) If an actual argument is a pointer array, and the
!*        corresponding dummy argument has either the VOLATILE or ASYNCHRONOUS
!*        attribute, that dummy argument shall be an assumed-shape array or
!*        a pointer array.
!* ===================================================================

  program volatileC1233PointerArray03

    interface
        subroutine ExplicitArray(x)
            class(*), VOLATILE :: x(*)
        end subroutine
    end interface

    class(*), allocatable :: ptrArray(:,:)

    allocate(ptrArray(2,2), source=reshape((/1,2,3,4/),(/2,2/)))

    call ExplicitArray(ptrArray)        !actual argument is allocatable array

    deallocate(ptrArray)

  end program volatileC1233PointerArray03

  subroutine ExplicitArray(x)
       class(*), VOLATILE :: x(*)   
  end subroutine ExplicitArray
