! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/mv_Alloc/uLimitPoly/unlimitpolyJ.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 06/08/2006
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : FROM and TO are unlimited polymorphic component of
!*                               a derived-type
!*                               similar to unlimitpolyH.f, but Y is not of unlimited
!*                               polymorphic, is polymorphic class(B)
!*                               Defect 321279
!*
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

    type A(n1,k1)    ! (20,4)
        integer, kind :: k1
        integer, len  :: n1
        integer(k1)      iA
    end type

    type, extends(A) :: B    ! (20,4)
        integer(k1) iB
        class(*), allocatable  :: b1(:)
    end type

    integer i

    class(*), allocatable :: x1(:), x2(:)
    class(B(:,4)), allocatable :: y(:)

    allocate(x1(5), source = (/ ( A(20,4)(i), i = 1, 5 ) /) )
    allocate(x2(3), source = (/ ( A(20,4)(i), i = 6,8 ) /) )

    allocate(y(2), source = (/ B(20,4)(iA=11, iB=101,b1=x1), B(20,4)(iA=12, iB=102, b1=x2 ) /))

    select type (y)
        type is (B(*,4))
            call move_alloc( y(1)%b1 , y(2)%b1 )
            if ( allocated(y(1)%b1 )) error stop 31
            if ( .not. allocated(y(2)%b1) ) error stop 33
            select type ( arg => y(2)%b1 )
        	type is (A(*,4))
                    print *, arg%iA
                class default
                   stop 41
            end select
        class default
           stop 51
    end select

   end
