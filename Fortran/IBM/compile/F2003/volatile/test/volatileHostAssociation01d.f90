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
!*                      An interface body has access via host association
!* to the named entities from its host that are made accessible by IMPORT 
!* statements in the interface body. The accessed entities are known by 
!* the same name and have the same attributes as in the host, except that 
!* an accessed entity may have the VOLATILE or ASYNCHRONOUS attribute 
!* even if the host entity does not. 
!* ===================================================================
   
   program volatileHostAssociation01d

     type dt
       sequence
        integer :: x 
        real ::    y 
     end type
     type(dt) :: b 
     byte     :: a
     interface
        subroutine sub(c)
           import dt
           import b, a
           type (dt) c 
           VOLATILE b 
           VOLATILE a
         end subroutine sub
      end interface

   end program volatileHostAssociation01d

