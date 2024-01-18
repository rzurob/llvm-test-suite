! GB DTP extension using:
! ftcx_dtp -qk -qnol -qreuse=base /tstdev/F2003/mv_Alloc/typCompatible/intkind4a.f
! opt variations: -qnok -ql -qreuse=none

! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : intkind4a.f 
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
!*  DESCRIPTION                : FROM is of type integer 
!*                               TO is of unlimited poly
!*                               multipul layers of select type to get TO
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

      type  :: base(k1)    ! (4)
          integer, kind :: k1
          class(*), allocatable ::  i1 
      end type 

      type, extends(base) :: child    ! (4)
          integer(k1), allocatable ::   i2
      end type

      class(base(4)), allocatable :: b

      allocate(child(4) :: b)

      select type ( b)
         type is (child(4))
            allocate(b%i2, source = 101 ) 
            allocate( base(4) :: b%i1)
      
            select type ( x => b%i1)
               type is ( base(4))
                  allocate( child(4) :: x%i1 )
                  select type ( y => x%i1 )
                     type is (child(4)) 
                        allocate ( y%i1, source = 99 )
                        call move_alloc(b%i2, y%i1)

			if ( .not. allocated(y%i1) ) stop 22

                        select type (z => y%i1)
                           type is (integer)
                              if ( z /= 101 ) stop 23
                           class default 
                              stop 25
                        end select
                     class default
                        stop 31
                  end select 
               class default
                  stop 41
            end select
            
            if ( allocated (b%i2) ) stop 31 
         class default
            stop 51
      end select
end
