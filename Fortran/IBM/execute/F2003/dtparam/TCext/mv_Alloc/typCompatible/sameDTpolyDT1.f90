! GB DTP extension using:
! ftcx_dtp -qck -qk -ql -qdeferredlp /tstdev/F2003/mv_Alloc/typCompatible/sameDTpolyDT1.f
! opt variations: -qnock -qnok -qnol -qnodeferredlp

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : sameDTpolyDT1.f 
!*
!*  PROGRAMMER                 : Michelle Zhang 
!*  DATE                       : 06/13/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : MOVE_ALLOC (FROM, TO)
!*                             :
!*  SECONDARY FUNCTIONS TESTED : 
!*                              
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : FROM is of an non-poly DT, an optional dummy
!*                               arg of a type bound proc
!*                               TO is of a poly DT with same declared type
!*                               as FROM
!*                               TO is a function return name
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

      type  :: base(k1,n1)    ! (4,20)
          integer, kind :: k1
          integer, len  :: n1
          character(:), allocatable :: ch 
          contains
              procedure :: get_alloc  => func 
      end type 

      contains 
         function func(arg, brg)
            class(base(4,*)) :: arg 
            class(base(4,:)) :: func
            type(base(4,:)), optional, allocatable :: brg 
            allocatable func

            call move_alloc(brg,func) 
            
         end function 

end module

      use m

      class(base(4,:)), allocatable :: b
      type(base(4,:)), allocatable :: c

      allocate(b, source=( base(4,20) ( 'IBM') ) )
      allocate(c, source=( base(4,20) ( 'IBMXLFortran') ) )

      select type ( x => b%get_alloc( c ))
          type is (base(4,*))
             if ( x%ch /= 'IBMXLFortran') STOP 21
          class default
             STOP 23
      end select
             
      end
