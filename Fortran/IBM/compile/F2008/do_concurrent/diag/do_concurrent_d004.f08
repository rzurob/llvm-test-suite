!*******************************************************************************
!*
!============================================================================
!*
!============================================================================
!*
!*  DATE                       : 2015-03-20
!*
!*  PRIMARY FUNCTIONS TESTED   : DO CONCURRENT (F2008 extension)
!*  SECONDARY FUNCTIONS TESTED :
!*  ADAPTED FROM               :
!*
!*  DESCRIPTION                :
!*   - do-stmt is a label-do-stmt, but the corresponding end-do is not
!*     identified with the same label
!*   - do-stmt is not a label-do-stmt, but the corresponding end-do is
!*     identified with a label
!*   - nesting the above, also interaction with other name-able constructions
!*
!=============================================================================
!2345678901234567890123456789012345678901234567890123456789012345678901234567890
      integer i, j

      Mothership: DO CONCURRENT (j = 0:1, i = 0:1)
        NamedIf1: if (i.eq.0) then
        end if NamedIf1
      END DO

      DO CONCURRENT (j = 0:1, i = 0:1)
        NamedIf2: if (i.eq.0) then
        end if NamedIf2
      END DO Unnamed

      Mothership2: DO CONCURRENT (integer :: j = 0:1, i = 0:1)
        NamedIf3: if (i.eq.0) then
        end if NamedIf3
      END DO

      DO CONCURRENT (integer :: j = 0:1, i = 0:1)
        NamedIf4: if (i.eq.0) then
        end if NamedIf4
      END DO Unnamed2

      UnnamedOuter1: DO CONCURRENT (integer :: j = 0:1)
        NamedIf5: if (i.eq.0) then
        end if NamedIf5
        NestedNamed1: DO CONCURRENT (integer :: i = 0:1)
        END DO NestedNamed1
      END DO

      DO CONCURRENT (integer :: j = 0:1)
        NestedNamed2: DO CONCURRENT (integer :: i = 0:1)
         NamedIf6: if (i.eq.0) then
         end if NamedIf6
        END DO NestedNamed2
      END DO UnnamedOuter2

      NamedOuter1: DO CONCURRENT (integer :: j = 0:1)
        UnnamedNested1: DO CONCURRENT (integer :: i = 0:1)
        END DO
        NamedIf7: if (i.eq.0) then
        end if NamedIf7
      END DO NamedOuter1

      NamedOuter2: DO CONCURRENT (integer :: j = 0:1)
        DO CONCURRENT (integer :: i = 0:1)
        END DO UnnamedNested2
      END DO NamedOuter2

      NamedOuter3: DO CONCURRENT (integer :: j = 0:1)
        DO i = 0, 1
        END DO UnnamedNestedDo
      END DO NamedOuter3

      UnnamedDo: DO j = 0,1
        NamedDoConcurrent: DO CONCURRENT( integer :: i = 0:1)
        END DO NamedDoConcurrent
      END DO

      UnnamedIf: if (i .eq. 0) then
        NamedDoConcurrent2: DO CONCURRENT( integer :: i = 0:1)
        END DO NamedDoConcurrent2
      end if

      SameName: DO i = 0,1
        SameName1: DO CONCURRENT ( integer :: j = 0:1)
        END DO SameName1
      END DO SameName

      SameName: BLOCK
      END BLOCK SameName

      SameName1: if (i .eq. 0) then
      end if SameName1
end
