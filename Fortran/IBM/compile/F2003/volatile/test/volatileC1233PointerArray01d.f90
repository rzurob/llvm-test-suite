!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : pointer array, VOLATILE
!*
!*  DESCRIPTION                : diagnostic TC for  C1233
!*
!*  C1233 (R1221) If an actual argument is a pointer array, and the
!*        corresponding dummy argument has either the VOLATILE or ASYNCHRONOUS
!*        attribute, that dummy argument shall be an assumed-shape array or
!*        a pointer array.
!* ===================================================================

  program volatileC1233PointerArray01d

    interface
        subroutine ExplicitArray(x)
            class(*),VOLATILE :: x(2,2)
        end subroutine
    end interface

    class(*), pointer :: ptrArray(:,:)

    allocate(ptrArray(2,2), source=reshape((/1,2,3,4/),(/2,2/)))

    call ExplicitArray(ptrArray)

    deallocate(ptrArray)

  end program volatileC1233PointerArray01d

  subroutine ExplicitArray(x)
       class(*), VOLATILE :: x(2,2)     ! dummy arg. is explicit shape array
  end subroutine ExplicitArray

