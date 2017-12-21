      subroutine convec
      implicit none
*
*.... 1995 JUL - CHANGED TO BRING INTO CONFORMITY WITH BOB'S atlas9v.for;83
*
*----------------------------- COMMONS ---------------------------------
*
      include 'common.sizebl'
      include 'common.constb'
*
      include 'common.abross'
      include 'common.convbl'
      include 'common.edenbl'
      include 'common.fluxbl'
      include 'common.height'
      include 'common.ifblkk'
      include 'common.ionsbl'
      include 'common.ptotal'
      include 'common.pzerob'
      include 'common.radblk'
      include 'common.rhoxbl'
      include 'common.stateb'
      include 'common.tempbl'
      include 'common.teffbl'
      include 'common.turbpr'
      include 'common.xabund'
*
*----------------------------- CONSTANTS ------------------------------
*
      double precision factre, factrm, factrp
      parameter ( factre = 1.0d0 / 0.999d0,
     &            factrm = 0.999d0 / 1.001d0,
     &            factrp = 1.001d0)
*
*--------------------------- LOCAL VARIABLES --------------------------
*
      double precision  abconv(maxd), cnv1, cnv2, cnvint(maxd), d, ddd, 
     &                  dedpg, dedt, del, delhgt(maxd), delta, 
     &                  deltat(maxd), dilut(maxd), dminus, down, dpdpg, 
     &                  dpdt, dplus, drdpg, drdt, dtdrhx(maxd), 
     &                  edens1(maxd), edens2(maxd), edens3(maxd), 
     &                  edens4(maxd), efactr, fluxco, heatcv, nuc, 
     &                  olddelt, rho1(maxd), rho2(maxd), rho3(maxd), 
     &                  rho4(maxd),rosst(maxd), rosstab, savxne(maxd), 
     &                  savxna(maxd), savrho(maxd), taub, term, up, 
     &                  vco, wtcnv, yc
      integer  ideltat, its30, j, l, m, map1, n
      logical  done, more
*!!!!      equivalence (dtdrhx(1), dltdlp(1))
*...........................................................................
*.... TO MAKE THE NOTATION HERE COMPATIBLE WITH THAT OF
*.... HENYEY ET AL. AND OF GUSTAFSSON,  I HAVE INTRODUCED
*.... THE FOLLOWING VARIABLES:
*....    BETA -  THE TERM FOR THE TURBULENT PRESSURE
*....            THIS IS IDENTICALLY 0 IN THIS ROUTINE,  SO IT
*....            IS LEFT OUT
*....    ALPHA - THE RATIO L / H KNOWN AS MIXLTH HERE
*....    NUC -   HENYEY'S NU  HERE TAKEN TO BE 8.0
*....    YC -    HENYEY'S Y  HERE TAKEN TO BE 0.5
*.......................................................................
      save nuc, yc
*
*-------------------------- EXTERNAL FUNCTIONS -------------------------
*
      external deriv, map1, pops, rosstab
*
*--------------------------- INITIALIZATION ---------------------------
*
      data  yc, nuc / 0.5d0, 8.0d0 /
*
*------------------------------ EXECUTION ------------------------------
*
	print *, ' first of all calculate derivs '
      call deriv (rhox, t, dtdrhx, (nrhox))
	print *, ' done '
      ifedns = .true.
*
*.... CALCULATE NUMERICAL DERIVATIVES BY EVALUATING FUNCTIONS AT + AND - 0.001
*
      do j = 1, nrhox
         dilut(j)  = 1.0d0 - exp(-tauros(j))
         savxne(j) = xne(j)
         savxna(j) = xnatom(j)
         savrho(j) = rho(j)
         tlog(j)   = tlog(j) + 0.0009995003d0
         t(j)      = t(j)    * factrp
         tk(j)     = tk(j)   * factrp 
         tkev(j)   = tkev(j) * factrp 
      end do
*
      itemp = itemp + 1
      call pops(0.0d0, 1, xne)
      efactr = 1.001d0 ** 4 - 1.0d0
*
      do j = 1, nrhox
*
*.... 3.0 * pradk IS APPROXIMATELY raden THE RADIATION DENSITY
*.... pradk IS USED BECAUSE IT CAN BE RECONSTRUCTED FROM MODEL DECKS
*.... WHEREAS raden CANNOT
*.... RIGOROUSLY THE RADIATION FIELD SHOULD BE RECALCULATED
*
         edens1(j) = edens(j) + 3.0d0 * pradk(j) / rho(j) * 
     &               (1.0d0 + dilut(j) * efactr )
         rho1(j) = rho(j)
         tlog(j) = tlog(j) - 0.0009995003d0 - 0.0010005003d0
         t(j)    = t(j)    * factrm 
         tk(j)   = tk(j)   * factrm
         tkev(j) = tkev(j) * factrm
      end do
*
      itemp = itemp + 1
      call pops(0.0d0, 1, xne)
      efactr = 0.999d0 ** 4 - 1.0d0
*
      do j = 1, nrhox
         edens2(j) = edens(j) + 3.0d0 * pradk(j) / rho(j) * 
     &               (1.0d0 + dilut(j) * efactr )
         rho2(j) = rho(j)
         tlog(j) = tlog(j) + 0.0010005003d0
         t(j)    = t(j)    * factre 
         tk(j)   = tk(j)   * factre
         tkev(j) = tkev(j) * factre
         p(j)    = p(j)    * factrp
      end do
*
      itemp = itemp + 1
      call pops(0.0d0, 1, xne)
*
      do j = 1, nrhox
         edens3(j) = edens(j) + 3.0d0 * pradk(j) / rho(j)
         rho3(j)   = rho(j)
         p(j)      = p(j) * factrm
      end do
*
      itemp = itemp + 1
      call pops(0.0d0, 1, xne)
*
      do j = 1, nrhox
         edens4(j) = edens(j) + 3.0d0 * pradk(j) / rho(j)
         rho4(j)   = rho(j)
         xne(j)    = savxne(j)
         xnatom(j) = savxna(j)
         rho(j)    = savrho(j)
         p(j)      = p(j) * factre
      end do
*
      do j = 1, nrhox
         dedt  = (edens1(j) - edens2(j)) / t(j) * 500.0d0
         drdt  = (rho1(j) - rho2(j))     / t(j) * 500.0d0
         dedpg = (edens3(j) - edens4(j)) / p(j) * 500.0d0
         drdpg = (rho3(j) - rho4(j))     / p(j) * 500.0d0
*
*.... CALCULATE THERMODYNAMIC QUANTITIES AND CONVECTIVE FLUX
*.... IGNORING pturb AND ASSUMING prad PROPORTIONAL TO T**4
*
         dpdpg     = 1.0d0
         dpdt      = 4.0d0 * pradk(j) / t(j) * dilut(j)
         dltdlp(j) = ptotal(j) / t(j) / grav * dtdrhx(j)
         heatcv    = dedt - dedpg * drdt / drdpg
         heatcp(j) = dedt - dedpg * dpdt / dpdpg - ptotal(j) / 
     &               rho(j) ** 2 * (drdt - drdpg * dpdt / dpdpg)
         velsnd(j) = sqrt(heatcp(j) / heatcv * dpdpg / drdpg)
         dlrdlt(j) = t(j) / rho(j) * (drdt - drdpg * dpdt / dpdpg)
         grdadb(j) = -ptotal(j) / rho(j) / t(j) * dlrdlt(j) / heatcp(j)
         hscale(j) = ptotal(j) / rho(j) / grav
         vconv(j)  = 0.0d0
         flxcnv(j) = 0.0d0
         abconv(j) = abross(j)
         deltat(j) = 0.0d0
         rosst(j)  = 0.0d0
*
         if (mixlth .gt. 0.0d0 .and. j .ge. 4) then
            del = dltdlp(j) - grdadb(j)
*
            if (del .ge. 0.0d0) then
               vco = 0.5 * mixlth * sqrt(-0.5d0 * ptotal(j) / 
     &               rho(j) * dlrdlt(j))
               fluxco = 0.5d0 * rho(j) * heatcp(j) * t(j) * 
     &                  mixlth / fourpi
               rosst(j) = rosstab(t(j), p(j), vturb(j))
*
*.... ITERATE ON THE OPACITY
*
	print *, ' iterate on opacities '
               if (ifconv) then
                  its30 = 30
*
               else
                  its30 = 1
               end if
*
               olddelt = 0
               ideltat = 0
               done = .false.
*
               do while (.not. done .and. ideltat .lt. its30)
                  ideltat = ideltat + 1
                  dplus = rosstab(t(j)+deltat(j), p(j), vturb(j)) / 
     &                    rosst(j)
                  dminus = rosstab(t(j)-deltat(j), p(j), vturb(j)) / 
     &                     rosst(j)
                  abconv(j) = 2.0d0 / (1.0d0 / dplus + 1.0 / dminus) * 
     &                        abross(j)
                  d = 8.0d0 * sigma * t(j) ** 4 / 
     &                (abconv(j) * hscale(j) * rho(j)) / 
     &                (fluxco * fourpi) / vco
*
*.... CORRECTION FOR OPTICALLY THIN BUBBLES AFTER MIHALAS
*
                  taub = abconv(j) * rho(j) * mixlth * hscale(j)
                  d = d * taub ** 2 / (2.0d0 + taub ** 2)
*
                  d = 0.5d0 * d ** 2
                  ddd = (del / (d + del)) ** 2
*
                  if (ddd .ge. 0.5) then
                     delta = (1.0d0 - sqrt(1.0d0 - ddd)) / ddd
*
                  else
                     delta =  0.5d0
                     term  =  0.5d0
                     up    = -1.0d0
                     down  =  2.0d0
                     more  = .true.
*
                     do while (more)
                        up    = 2.0d0 + up
                        down  = 2.0d0 + down
                        term  = up / down * ddd * term
                        delta = delta + term
                        if(term .le. 1.0d-6) more = .false.
                     end do
*
                  end if
*
                  delta     = delta * del ** 2 / (d + del)
                  vconv(j)  = vco * sqrt(delta)
                  flxcnv(j) = fluxco * vconv(j) * delta
                  deltat(j) = t(j) * mixlth * delta
                  deltat(j) = min(deltat(j), t(j) * 0.15)
                  deltat(j) = deltat(j) * 0.7 + olddelt * 0.3d0
                  if(abs(deltat(j) - olddelt) .lt. 0.5) done = .true.
                  olddelt   = deltat(j)
               end do
*
            end if
*
         end if
*
      end do
	print *, ' done with opacity stuff '
*
*.... ELIMINATE ARTIFACTUAL CONVECTION ABOVE THE MAIN CONVECTION ZONE
*.... REWRITE CODE IF THERE ARE TWO CONVECTION ZONES
*
      n = nrhox - 2
*
      do while (flxcnv(n) .le. 0.0d0 .and. n .gt. 1)
         n = n - 1
      end do
*
      if (n .gt. 1) then
         l = n
*
         do while (flxcnv(l) .gt. 0.0d0 .and. l .gt. 1)
            l = l - 1
         end do
*
         do j = 1, l
            vconv(j)  = 0.0d0
            flxcnv(j) = 0.0d0
         end do
*
         do j = 1, nrhox
            flxcnv0(j) = flxcnv(j)
         end do
*
*.... ASSUME OVERSHOOTING BY 0.5 hscale IF CONVECTION IS STRONG
*.... BUT NONE IF CONVECTION IS WEAK
*.... SETTING OVERWT = 0.0 COMPLETELY TURNS OFF OVERSHOOTING
*
         if (overwt .gt. 0.0d0) then
*
*.... CORRECTION FROM FIORELLA CASTELLI
*
	print *, ' turning of overshooting '
            wtcnv = 0.0d0
*
            do j = 1, nrhox
               wtcnv = max(wtcnv, flxcnv(j)/flux)
            end do
*
            wtcnv = min(wtcnv, 1.0d0) * overwt
*
            do j = 1, nrhox
               delhgt(j) = min(hscale(j) * 0.5d-5 * wtcnv, 
     &                         height(nrhox) - height(j),
     &                         height(j) - height(1))
               flxcnv0(j) = flxcnv(j)
               flxcnv1(j) = 0.0d0
            end do
*
            call integ(height, flxcnv, cnvint, nrhox, 0.0d0)
*
            do j = nrhox / 2, nrhox - 1
*
               if (delhgt(j) .ne. 0.0d0) then
                  m = map1(height, cnvint, nrhox, height(j)-delhgt(j),
     &                     cnv1, 1)
                  m = map1(height, cnvint, nrhox, height(j)+delhgt(j),
     &                     cnv2, 1)
                  flxcnv1(j) = flxcnv1(j) + 0.5 * (cnv2 - cnv1) / 
     &                         delhgt(j)
               end if
*
            end do
*
            do j = 1, nrhox
               flxcnv(j) = max(flxcnv0(j), flxcnv1(j))
            end do
*
         end if
*
*.... PATCH TO REMOVE NUMERICAL ARTIFACTS
*
         do j = 1, nrhox / 2
            flxcnv(j) = 0.0d0
         end do
*
      end if
*
      end
*
*********** E N D   O F   S U B R O U T I N E   C O N V E C ************
*
