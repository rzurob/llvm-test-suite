! GB DTP extension using:
! ftcx_dtp -qnol /tstdev/F2003/mv_Alloc/typCompatible/complex32.f
! opt variations: -ql

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/13/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : TO is of unlimited poly, component of a DT
!*                               FROM is of complex*32 with private attr
!*                               be called in external procedure
!*                               rank 4
!*
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

       type A(k1)    ! (16)
          integer, kind            :: k1
          complex(k1), allocatable :: a1(:,:,:,:)
          class(*), allocatable :: a2(:,:,:,:)
       end type

       interface
          subroutine sub(arg)
              import A
              type(A(16)), intent(inout) :: arg
          end subroutine
       end interface
end module

program main

       use m
       type(A(16)) ::  aT

       aT%a1 = reshape( (/ ( cmplx(i+1, i-1, 16), i = -7, 8 ) /),(/2,2,2,2/) )

       allocate( complex*8 :: aT%a2(5,5,5,-1))
       select type( x=>aT%a2 )
           type is (complex(16))
               x = cmplx( int(2, 1), real(3.0, 16), 16 )
       end select

       call sub(aT)

       if ( .not. allocated(aT%a2) ) error stop 21
       if ( allocated(aT%a1) ) error stop 23

       do i = 1, 4
           if ( size( aT%a2, i) /= 2 ) call zzrc(i)
       end do

       select type( x=>aT%a2 )
           type is (complex(16))
           write ( 6, 100) x(1,1,2,2)
       end select

       100 format ( "(", 2f20.17, ")")
end

subroutine sub(arg)
    use m, only : A

    type(A(16)), intent(inout) :: arg

    if ( .not. allocated(arg%a1) ) then
         call move_alloc(arg%a1, arg%a1)
    else
         call move_alloc(arg%a1, arg%a2)
    end if

end subroutine
