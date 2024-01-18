!*    ********************************************************************
!*                          CLASSIFY KERNEL                              *
!*    ********************************************************************

      subroutine kmeans_classify(N,K,D,pointv,lnearestv,clusterv)
      implicit none
      integer*8 N, K, D
      double precision pointv(N,D)[*]
      double precision clusterv(K,D)
      integer*8 lnearestv(N)
      integer*8 n1,k1,d1,kmin0
      double precision mindist0,d0,diff0

      do n1=1,N
         mindist0 = 1.0e99
         kmin0 = -1
         do k1=1,K
            d0 = 0.0
            do d1=1,D
               diff0 = pointv(n1,d1)-clusterv(k1,d1)
               d0 = d0 + diff0*diff0
            end do
            if (d0 .lt. mindist0) then
               kmin0 = k1
               mindist0 = d0
            end if
         end do
         lnearestv(n1) = kmin0
      end do
      end

