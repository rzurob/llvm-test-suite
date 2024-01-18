!*  ===================================================================
!*
!*  DATE                       : 30/05/2006
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

   program volatileHostAssociation02d

    interface
       subroutine sub1(a)
         type dt(k, l)
          integer, kind :: k
          integer, len ::  l
          sequence
          integer(k) :: arr(l)
         end type

         type(dt(8,8)) ::a          ! type parameter
           real     :: b
           interface
               subroutine sub2()
                  import a, b       ! import type parameter
                  VOLATILE a        ! declare it as VOLATILE
                  VOLATILE b
               end subroutine
           end interface
        end subroutine
     end interface

   end program volatileHostAssociation02d

