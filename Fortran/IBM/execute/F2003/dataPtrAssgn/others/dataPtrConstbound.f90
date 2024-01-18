!*********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrConstbound.f
!*
!*  DATE                       : Aug 31, 2006
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION
!*
!* - typeless literal constants as lb/ub
!* - data-pointer is not poly, data-target is poly with different dynamic type
!*   the assignment target is the ancestor component of data-target
!* - type(base) ptr => class(base) target
!* - class(base) ptr1 => type(base) ptr
!*
!234567890123456789012345678901234567890123456789012345678901234567890

        module m
		type base
		    integer age
		    character(:), allocatable :: name
		end type

		type, extends(base) :: child
		end type

		type, extends(child) :: grandchild
		end type

        end module

        program main
                use m

		type(base), pointer :: b1(:,:)
		class(base), target, allocatable :: c1(:,:)
	 	class(child), allocatable :: get_info(:)
                class(base), pointer :: b2(:)

		allocate(get_info(60))
		if ( .not. allocated(get_info) ) stop 3

		do i = 1, 60
			if ( mod(i,5) == 1 )  get_info(i)%age = 18
			if ( mod(i,5) == 2 )  get_info(i)%age = 19
			if ( mod(i,5) == 3 )  get_info(i)%age = 20
			if ( mod(i,5) == 4 )  get_info(i)%age = 21
			if ( mod(i,5) == 0 )  get_info(i)%age = 22

			select case(mod(i,10))
			    case(0)
				get_info(i)%name = 'Bob'
			    case(1)
				get_info(i)%name = 'Tom'
			    case(2)
				get_info(i)%name = 'Lucy'
			    Case(3)
				get_info(i)%name = 'Sandy'
			    case(4)
				get_info(i)%name = 'Kate'
			    case(5)
				get_info(i)%name = 'King'
			    case(6)
				get_info(i)%name = 'Smith'
			    case(7)
				get_info(i)%name = 'Law'
			    case(8)
				get_info(i)%name = 'Ng'
			    case(9)
				get_info(i)%name = 'Stone'
			end select

		end do

		allocate(c1(10,6),source=reshape(   &
                  (/(grandchild(get_info(i)%age,get_info(i)%name),i=1,60)/), &
                              (/10,6/)))

		if ( .not. allocated(c1) ) stop 4

		!do i = 1, 60
           	 !    print *, get_info(i)%name, get_info(i)%age
		!enddo

		! lb is constant
		b1(o'10':, B'101':) => c1

		if (any(lbound(b1) .ne. (/8,5/))) stop 5
		if (any(ubound(b1) .ne. (/17,10/))) stop 7
		if ( .not. associated(b1, c1)) stop 9

		! lb/up is constant
		b2("13"o:z'13') => b1(:,6)
		if ( .not. associated(b2)) stop 10

		b2(1:) => b2

		if ( lbound(b2,1) /= 1 ) stop 11
		if ( ubound(b2,1) /= 9 ) stop 13
		if ( .not. associated(b2)) stop 15

	 	select type(b2)
	            type is (base)
			do i = 1,9
  	            	     print *, b2(i)%name, b2(i)%age
			enddo
		    class default
			stop 21
		end select

        End program
