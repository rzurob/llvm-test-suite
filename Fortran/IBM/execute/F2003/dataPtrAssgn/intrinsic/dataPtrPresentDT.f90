!*********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*
!*  TEST CASE NAME             : dataPtrPresentDT.f 
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
!* - data-pointer of non-poly type, type cmp with data-target of poly type
!* - data-pointer used as arg of present
!* - data-pointer has allocatable attribute
!* - after ptr =, apply intrinsin = to data-pointer p = p
!*
!234567890123456789012345678901234567890123456789012345678901234567890

module m

    type A 
        complex, allocatable :: x 
    end type

    interface   
        subroutine sub(p)       
            import A    
            type(A), optional, pointer :: p(:)  
        end subroutine  
    end interface 

end module

    program main

        use m
        type(A), pointer :: p(:)
        class(A), pointer :: tar(:)

        allocate(tar(10), source = (/ ( A(cmplx(i,i*2,4)), i=1,10) /))

        p(1:) => tar(::2)

        if ( .not. associated(p, tar(::2))) stop 11
        if ( lbound(p,1) /= 1 ) stop 13
        if ( ubound(p,1) /= 5 ) stop 15

        call sub(p)

        do i = 1, 5
            write (*, '("(",f10.6,", ", f10.6, ")")') p(i)%x 
        enddo

    end program

    subroutine sub(p)       
      use m, only: A              
      type(A), optional, pointer :: p(:)  

      if ( present(p) ) then      
        p = p   
      endif       

    end subroutine
