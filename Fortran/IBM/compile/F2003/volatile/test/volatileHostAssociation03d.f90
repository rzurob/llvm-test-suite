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
!*  PRIMARY FUNCTIONS TESTED   : Host Association, VOLATILE
!*  DESCRIPTION                :
!*        16.4.1.3 
!* An internal subprogram, a module subprogram, or a derived-type
!* definition has access to the named entities from its host via
!* host association. An interface body has access via host association
!* to the named entities from its host that are made accessible by IMPORT 
!* statements in the interface body. The accessed entities are known by 
!* the same name and have the same attributes as in the host, except that 
!* an accessed entity may have the VOLATILE or ASYNCHRONOUS attribute 
!* even if the host entity does not. 
!* ===================================================================
   
   program volatileHostAssociation03d

     type dt
        character(:), allocatable :: x
     end type

     character(4) :: ch

     type(dt) :: d

     ch  = "IBM"

     allocate(d%x, source=ch)

     call sub()

        contains

           subroutine sub()
               VOLATILE :: d
               print *, d%x
           end subroutine sub

   end program volatileHostAssociation03d

