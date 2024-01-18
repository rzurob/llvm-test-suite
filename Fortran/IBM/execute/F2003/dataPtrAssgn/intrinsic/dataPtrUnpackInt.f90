!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrUnpackInt.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : Aug 31, 2006
!*  ORIGIN                     : Compiler Development, IBM Toronto Lab
!*
!*  PRIMARY FUNCTIONS TESTED   : Pointer Assignment Enhancement 
!*
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION
!*
!* - data-pointer, type class(*), a component of DT
!* - data-target, type integer(4), a component of same DT as data-pointer
!* - DT contains third component of type integer*8, with allocatable attri 
!*
!234567890123456789012345678901234567890123456789012345678901234567890

    type base
        class(*), pointer :: p(:)
        integer*4 :: t4(10)
        integer*8, allocatable :: t8(:)
    end type

        type(base),target :: b

        b%t4 = (/( i,i=1,10)/)

        b%p(1:) => b%t4

        if ( .not. associated(b%p, b%t4)) stop 5
        if ( lbound(b%p,1) /= 1 ) stop 7 
        if ( ubound(b%p,1) /= 10 ) stop 9 

        select type(x => b%p)
            type is (integer)
                print *, x
                print *, unpack(x,mod(x,2)==0,x(10:1:-1))
            class default
                stop 11
        end select

    end program
