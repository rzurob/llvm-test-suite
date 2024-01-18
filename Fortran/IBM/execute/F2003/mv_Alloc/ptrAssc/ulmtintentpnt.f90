! *********************************************************************
!*  =================================================================== 
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY 
!*  =================================================================== 
!*  =================================================================== 
!*
!*  TEST CASE TITLE            : ulmtintentpnt.f 
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
!*  DESCRIPTION                : 
!*                               TO is dummy arg with intent(out) attr 
!*                               pointer is dummay arg with intent(inout) 
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012


    character(:), allocatable, target :: FROM(:), TO(:)
    class(*), pointer :: P(:)

    allocate ( from(3), source = (/ '1234', 'abcd', 'wxyz'/))

    call sub(p, to)

    if ( allocated(from)) stop 11
    if ( .not. allocated(to) ) stop 13

    print *, TO

    if ( .not. associated(P, to )) stop 21

    contains
        subroutine sub(p, to)
            class(*), pointer, intent(inout) :: p(:)
            character(:), intent(out), allocatable :: to(:)
        
            p => from
    
            call move_alloc (from, to)
        end subroutine 
    end
