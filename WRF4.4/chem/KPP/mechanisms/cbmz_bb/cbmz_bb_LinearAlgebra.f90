! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Linear Algebra Data and Routines File
! 
! Generated by KPP-2.1 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : cbmz_bb_LinearAlgebra.f90
! Time                 : Mon Sep 26 23:36:56 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/cbmz_bb
! Equation file        : cbmz_bb.kpp
! Output root filename : cbmz_bb
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE cbmz_bb_LinearAlgebra

  USE cbmz_bb_Parameters
  USE cbmz_bb_JacobianSP

  IMPLICIT NONE

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! KppSolveTR - sparse, transposed back substitution
!   Arguments :
!      JVS       - sparse Jacobian of variables
!      X         - Vector for variables
!      XX        - Vector for output variables
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE KppSolveTR ( JVS, X, XX )

! JVS - sparse Jacobian of variables
  REAL(kind=dp) :: JVS(LU_NONZERO)
! X - Vector for variables
  REAL(kind=dp) :: X(NVAR)
! XX - Vector for output variables
  REAL(kind=dp) :: XX(NVAR)

  XX(1) = X(1)/JVS(1)
  XX(2) = X(2)/JVS(4)
  XX(3) = X(3)/JVS(5)
  XX(4) = X(4)/JVS(6)
  XX(5) = X(5)/JVS(10)
  XX(6) = X(6)/JVS(18)
  XX(7) = (X(7)-JVS(2)*XX(1))/(JVS(20))
  XX(8) = X(8)/JVS(22)
  XX(9) = X(9)/JVS(24)
  XX(10) = X(10)/JVS(27)
  XX(11) = X(11)/JVS(30)
  XX(12) = X(12)/JVS(32)
  XX(13) = X(13)/JVS(35)
  XX(14) = X(14)/JVS(37)
  XX(15) = X(15)/JVS(42)
  XX(16) = X(16)/JVS(47)
  XX(17) = X(17)/JVS(51)
  XX(18) = X(18)/JVS(59)
  XX(19) = X(19)/JVS(62)
  XX(20) = X(20)/JVS(74)
  XX(21) = X(21)/JVS(78)
  XX(22) = X(22)/JVS(82)
  XX(23) = (X(23)-JVS(7)*XX(4))/(JVS(87))
  XX(24) = X(24)/JVS(90)
  XX(25) = X(25)/JVS(97)
  XX(26) = (X(26)-JVS(43)*XX(15))/(JVS(104))
  XX(27) = X(27)/JVS(109)
  XX(28) = X(28)/JVS(119)
  XX(29) = (X(29)-JVS(63)*XX(19))/(JVS(135))
  XX(30) = (X(30)-JVS(120)*XX(28))/(JVS(152))
  XX(31) = (X(31)-JVS(136)*XX(29))/(JVS(157))
  XX(32) = (X(32)-JVS(137)*XX(29))/(JVS(162))
  XX(33) = (X(33)-JVS(121)*XX(28))/(JVS(167))
  XX(34) = (X(34)-JVS(8)*XX(4)-JVS(11)*XX(5)-JVS(38)*XX(14)-JVS(52)*XX(17)-JVS(64)*XX(19)-JVS(91)*XX(24)-JVS(122)*XX(28)&
             &-JVS(138)*XX(29))/(JVS(172))
  XX(35) = (X(35)-JVS(12)*XX(5)-JVS(123)*XX(28)-JVS(158)*XX(31)-JVS(163)*XX(32))/(JVS(176))
  XX(36) = (X(36)-JVS(110)*XX(27)-JVS(124)*XX(28))/(JVS(188))
  XX(37) = X(37)/JVS(206)
  XX(38) = X(38)/JVS(219)
  XX(39) = (X(39)-JVS(13)*XX(5)-JVS(39)*XX(14)-JVS(53)*XX(17)-JVS(65)*XX(19)-JVS(92)*XX(24)-JVS(125)*XX(28)-JVS(139)&
             &*XX(29)-JVS(189)*XX(36)-JVS(207)*XX(37)-JVS(220)*XX(38))/(JVS(231))
  XX(40) = (X(40)-JVS(54)*XX(17)-JVS(79)*XX(21))/(JVS(239))
  XX(41) = (X(41)-JVS(66)*XX(19)-JVS(140)*XX(29)-JVS(190)*XX(36))/(JVS(250))
  XX(42) = (X(42)-JVS(111)*XX(27)-JVS(126)*XX(28)-JVS(208)*XX(37))/(JVS(262))
  XX(43) = (X(43)-JVS(112)*XX(27)-JVS(127)*XX(28))/(JVS(282))
  XX(44) = (X(44)-JVS(75)*XX(20)-JVS(93)*XX(24)-JVS(191)*XX(36))/(JVS(298))
  XX(45) = (X(45)-JVS(14)*XX(5)-JVS(113)*XX(27)-JVS(128)*XX(28)-JVS(141)*XX(29)-JVS(168)*XX(33)-JVS(192)*XX(36)-JVS(209)&
             &*XX(37)-JVS(221)*XX(38)-JVS(263)*XX(42)-JVS(283)*XX(43)-JVS(299)*XX(44))/(JVS(313))
  XX(46) = (X(46)-JVS(193)*XX(36)-JVS(222)*XX(38)-JVS(264)*XX(42)-JVS(300)*XX(44))/(JVS(322))
  XX(47) = (X(47)-JVS(67)*XX(19)-JVS(142)*XX(29)-JVS(210)*XX(37)-JVS(223)*XX(38)-JVS(240)*XX(40)-JVS(265)*XX(42)&
             &-JVS(284)*XX(43)-JVS(301)*XX(44)-JVS(323)*XX(46))/(JVS(336))
  XX(48) = (X(48)-JVS(68)*XX(19)-JVS(143)*XX(29)-JVS(211)*XX(37)-JVS(224)*XX(38)-JVS(241)*XX(40)-JVS(285)*XX(43)&
             &-JVS(302)*XX(44)-JVS(324)*XX(46)-JVS(337)*XX(47))/(JVS(353))
  XX(49) = (X(49)-JVS(69)*XX(19)-JVS(144)*XX(29)-JVS(212)*XX(37)-JVS(225)*XX(38)-JVS(242)*XX(40)-JVS(251)*XX(41)&
             &-JVS(286)*XX(43)-JVS(303)*XX(44)-JVS(325)*XX(46)-JVS(338)*XX(47)-JVS(354)*XX(48))/(JVS(369))
  XX(50) = (X(50)-JVS(3)*XX(1)-JVS(21)*XX(7)-JVS(23)*XX(8)-JVS(25)*XX(9)-JVS(31)*XX(11)-JVS(36)*XX(13)-JVS(40)*XX(14)&
             &-JVS(44)*XX(15)-JVS(48)*XX(16)-JVS(55)*XX(17)-JVS(60)*XX(18)-JVS(70)*XX(19)-JVS(76)*XX(20)-JVS(80)*XX(21)&
             &-JVS(83)*XX(22)-JVS(88)*XX(23)-JVS(94)*XX(24)-JVS(105)*XX(26)-JVS(114)*XX(27)-JVS(129)*XX(28)-JVS(145)*XX(29)&
             &-JVS(153)*XX(30)-JVS(164)*XX(32)-JVS(169)*XX(33)-JVS(173)*XX(34)-JVS(177)*XX(35)-JVS(194)*XX(36)-JVS(213)&
             &*XX(37)-JVS(226)*XX(38)-JVS(232)*XX(39)-JVS(243)*XX(40)-JVS(252)*XX(41)-JVS(266)*XX(42)-JVS(287)*XX(43)&
             &-JVS(304)*XX(44)-JVS(314)*XX(45)-JVS(326)*XX(46)-JVS(339)*XX(47)-JVS(355)*XX(48)-JVS(370)*XX(49))/(JVS(413))
  XX(51) = (X(51)-JVS(9)*XX(4)-JVS(15)*XX(5)-JVS(19)*XX(6)-JVS(41)*XX(14)-JVS(56)*XX(17)-JVS(71)*XX(19)-JVS(89)*XX(23)&
             &-JVS(95)*XX(24)-JVS(98)*XX(25)-JVS(130)*XX(28)-JVS(146)*XX(29)-JVS(154)*XX(30)-JVS(174)*XX(34)-JVS(178)*XX(35)&
             &-JVS(195)*XX(36)-JVS(214)*XX(37)-JVS(227)*XX(38)-JVS(233)*XX(39)-JVS(244)*XX(40)-JVS(253)*XX(41)-JVS(267)&
             &*XX(42)-JVS(288)*XX(43)-JVS(305)*XX(44)-JVS(315)*XX(45)-JVS(327)*XX(46)-JVS(340)*XX(47)-JVS(356)*XX(48)&
             &-JVS(371)*XX(49)-JVS(414)*XX(50))/(JVS(428))
  XX(52) = (X(52)-JVS(28)*XX(10)-JVS(33)*XX(12)-JVS(45)*XX(15)-JVS(49)*XX(16)-JVS(84)*XX(22)-JVS(99)*XX(25)-JVS(115)&
             &*XX(27)-JVS(372)*XX(49)-JVS(415)*XX(50)-JVS(429)*XX(51))/(JVS(462))
  XX(53) = (X(53)-JVS(16)*XX(5)-JVS(29)*XX(10)-JVS(306)*XX(44)-JVS(416)*XX(50)-JVS(430)*XX(51)-JVS(463)*XX(52))&
             &/(JVS(483))
  XX(54) = (X(54)-JVS(34)*XX(12)-JVS(46)*XX(15)-JVS(72)*XX(19)-JVS(100)*XX(25)-JVS(106)*XX(26)-JVS(116)*XX(27)-JVS(131)&
             &*XX(28)-JVS(147)*XX(29)-JVS(155)*XX(30)-JVS(159)*XX(31)-JVS(175)*XX(34)-JVS(179)*XX(35)-JVS(196)*XX(36)&
             &-JVS(215)*XX(37)-JVS(228)*XX(38)-JVS(234)*XX(39)-JVS(245)*XX(40)-JVS(254)*XX(41)-JVS(268)*XX(42)-JVS(289)&
             &*XX(43)-JVS(307)*XX(44)-JVS(316)*XX(45)-JVS(328)*XX(46)-JVS(341)*XX(47)-JVS(357)*XX(48)-JVS(373)*XX(49)&
             &-JVS(417)*XX(50)-JVS(431)*XX(51)-JVS(464)*XX(52)-JVS(484)*XX(53))/(JVS(510))
  XX(55) = (X(55)-JVS(17)*XX(5)-JVS(26)*XX(9)-JVS(50)*XX(16)-JVS(77)*XX(20)-JVS(81)*XX(21)-JVS(85)*XX(22)-JVS(117)&
             &*XX(27)-JVS(148)*XX(29)-JVS(160)*XX(31)-JVS(165)*XX(32)-JVS(170)*XX(33)-JVS(197)*XX(36)-JVS(216)*XX(37)&
             &-JVS(229)*XX(38)-JVS(246)*XX(40)-JVS(255)*XX(41)-JVS(269)*XX(42)-JVS(290)*XX(43)-JVS(308)*XX(44)-JVS(317)&
             &*XX(45)-JVS(329)*XX(46)-JVS(342)*XX(47)-JVS(358)*XX(48)-JVS(374)*XX(49)-JVS(418)*XX(50)-JVS(432)*XX(51)&
             &-JVS(465)*XX(52)-JVS(485)*XX(53)-JVS(511)*XX(54))/(JVS(550))
  XX(56) = (X(56)-JVS(61)*XX(18)-JVS(73)*XX(19)-JVS(86)*XX(22)-JVS(101)*XX(25)-JVS(132)*XX(28)-JVS(149)*XX(29)-JVS(156)&
             &*XX(30)-JVS(161)*XX(31)-JVS(166)*XX(32)-JVS(171)*XX(33)-JVS(198)*XX(36)-JVS(217)*XX(37)-JVS(230)*XX(38)&
             &-JVS(247)*XX(40)-JVS(256)*XX(41)-JVS(270)*XX(42)-JVS(291)*XX(43)-JVS(309)*XX(44)-JVS(318)*XX(45)-JVS(330)&
             &*XX(46)-JVS(343)*XX(47)-JVS(359)*XX(48)-JVS(375)*XX(49)-JVS(419)*XX(50)-JVS(433)*XX(51)-JVS(466)*XX(52)&
             &-JVS(486)*XX(53)-JVS(512)*XX(54)-JVS(551)*XX(55))/(JVS(576))
  XX(56) = XX(56)
  XX(55) = XX(55)-JVS(575)*XX(56)
  XX(54) = XX(54)-JVS(549)*XX(55)-JVS(574)*XX(56)
  XX(53) = XX(53)-JVS(509)*XX(54)-JVS(548)*XX(55)-JVS(573)*XX(56)
  XX(52) = XX(52)-JVS(482)*XX(53)-JVS(508)*XX(54)-JVS(547)*XX(55)-JVS(572)*XX(56)
  XX(51) = XX(51)-JVS(461)*XX(52)-JVS(481)*XX(53)-JVS(507)*XX(54)-JVS(546)*XX(55)-JVS(571)*XX(56)
  XX(50) = XX(50)-JVS(427)*XX(51)-JVS(460)*XX(52)-JVS(480)*XX(53)-JVS(506)*XX(54)-JVS(545)*XX(55)-JVS(570)*XX(56)
  XX(49) = XX(49)-JVS(412)*XX(50)-JVS(459)*XX(52)-JVS(479)*XX(53)-JVS(505)*XX(54)-JVS(544)*XX(55)-JVS(569)*XX(56)
  XX(48) = XX(48)-JVS(368)*XX(49)-JVS(411)*XX(50)-JVS(458)*XX(52)-JVS(478)*XX(53)-JVS(504)*XX(54)-JVS(543)*XX(55)&
             &-JVS(568)*XX(56)
  XX(47) = XX(47)-JVS(352)*XX(48)-JVS(410)*XX(50)-JVS(457)*XX(52)-JVS(477)*XX(53)-JVS(503)*XX(54)-JVS(542)*XX(55)&
             &-JVS(567)*XX(56)
  XX(46) = XX(46)-JVS(335)*XX(47)-JVS(409)*XX(50)-JVS(456)*XX(52)-JVS(476)*XX(53)-JVS(502)*XX(54)-JVS(541)*XX(55)&
             &-JVS(566)*XX(56)
  XX(45) = XX(45)-JVS(321)*XX(46)-JVS(334)*XX(47)-JVS(351)*XX(48)-JVS(367)*XX(49)-JVS(408)*XX(50)-JVS(426)*XX(51)&
             &-JVS(455)*XX(52)-JVS(475)*XX(53)-JVS(501)*XX(54)-JVS(540)*XX(55)-JVS(565)*XX(56)
  XX(44) = XX(44)-JVS(407)*XX(50)-JVS(454)*XX(52)-JVS(500)*XX(54)-JVS(539)*XX(55)-JVS(564)*XX(56)
  XX(43) = XX(43)-JVS(297)*XX(44)-JVS(406)*XX(50)-JVS(453)*XX(52)-JVS(474)*XX(53)-JVS(499)*XX(54)-JVS(538)*XX(55)
  XX(42) = XX(42)-JVS(405)*XX(50)-JVS(452)*XX(52)-JVS(473)*XX(53)-JVS(498)*XX(54)-JVS(537)*XX(55)-JVS(563)*XX(56)
  XX(41) = XX(41)-JVS(281)*XX(43)-JVS(350)*XX(48)-JVS(366)*XX(49)-JVS(404)*XX(50)-JVS(451)*XX(52)-JVS(497)*XX(54)&
             &-JVS(536)*XX(55)-JVS(562)*XX(56)
  XX(40) = XX(40)-JVS(280)*XX(43)-JVS(403)*XX(50)-JVS(450)*XX(52)-JVS(496)*XX(54)-JVS(535)*XX(55)-JVS(561)*XX(56)
  XX(39) = XX(39)-JVS(238)*XX(40)-JVS(249)*XX(41)-JVS(261)*XX(42)-JVS(279)*XX(43)-JVS(296)*XX(44)-JVS(320)*XX(46)&
             &-JVS(349)*XX(48)-JVS(402)*XX(50)-JVS(425)*XX(51)-JVS(449)*XX(52)-JVS(472)*XX(53)-JVS(495)*XX(54)-JVS(534)&
             &*XX(55)-JVS(560)*XX(56)
  XX(38) = XX(38)-JVS(295)*XX(44)-JVS(319)*XX(46)-JVS(401)*XX(50)-JVS(471)*XX(53)
  XX(37) = XX(37)-JVS(448)*XX(52)-JVS(494)*XX(54)-JVS(533)*XX(55)-JVS(559)*XX(56)
  XX(36) = XX(36)-JVS(400)*XX(50)-JVS(447)*XX(52)-JVS(493)*XX(54)-JVS(532)*XX(55)
  XX(35) = XX(35)-JVS(187)*XX(36)-JVS(205)*XX(37)-JVS(278)*XX(43)-JVS(312)*XX(45)-JVS(333)*XX(47)-JVS(348)*XX(48)&
             &-JVS(365)*XX(49)-JVS(399)*XX(50)-JVS(424)*XX(51)-JVS(446)*XX(52)-JVS(470)*XX(53)-JVS(492)*XX(54)-JVS(531)&
             &*XX(55)-JVS(558)*XX(56)
  XX(34) = XX(34)-JVS(186)*XX(36)-JVS(204)*XX(37)-JVS(237)*XX(40)-JVS(248)*XX(41)-JVS(260)*XX(42)-JVS(277)*XX(43)&
             &-JVS(294)*XX(44)-JVS(347)*XX(48)-JVS(398)*XX(50)-JVS(423)*XX(51)-JVS(469)*XX(53)-JVS(491)*XX(54)-JVS(530)&
             &*XX(55)
  XX(33) = XX(33)-JVS(185)*XX(36)-JVS(218)*XX(38)-JVS(259)*XX(42)-JVS(276)*XX(43)-JVS(332)*XX(47)-JVS(397)*XX(50)&
             &-JVS(445)*XX(52)-JVS(529)*XX(55)-JVS(557)*XX(56)
  XX(32) = XX(32)-JVS(184)*XX(36)-JVS(311)*XX(45)-JVS(331)*XX(47)-JVS(346)*XX(48)-JVS(364)*XX(49)-JVS(396)*XX(50)&
             &-JVS(444)*XX(52)-JVS(528)*XX(55)-JVS(556)*XX(56)
  XX(31) = XX(31)-JVS(275)*XX(43)-JVS(310)*XX(45)-JVS(345)*XX(48)-JVS(363)*XX(49)-JVS(395)*XX(50)-JVS(443)*XX(52)&
             &-JVS(527)*XX(55)-JVS(555)*XX(56)
  XX(30) = XX(30)-JVS(183)*XX(36)-JVS(203)*XX(37)-JVS(258)*XX(42)-JVS(274)*XX(43)-JVS(394)*XX(50)-JVS(422)*XX(51)&
             &-JVS(468)*XX(53)-JVS(526)*XX(55)
  XX(29) = XX(29)-JVS(344)*XX(48)-JVS(393)*XX(50)
  XX(28) = XX(28)-JVS(392)*XX(50)-JVS(525)*XX(55)
  XX(27) = XX(27)-JVS(391)*XX(50)-JVS(442)*XX(52)-JVS(490)*XX(54)
  XX(26) = XX(26)-JVS(108)*XX(27)-JVS(151)*XX(30)-JVS(202)*XX(37)-JVS(362)*XX(49)-JVS(390)*XX(50)-JVS(441)*XX(52)&
             &-JVS(489)*XX(54)-JVS(524)*XX(55)
  XX(25) = XX(25)-JVS(421)*XX(51)-JVS(440)*XX(52)-JVS(488)*XX(54)-JVS(554)*XX(56)
  XX(24) = XX(24)-JVS(182)*XX(36)-JVS(389)*XX(50)-JVS(523)*XX(55)
  XX(23) = XX(23)-JVS(118)*XX(28)-JVS(181)*XX(36)-JVS(201)*XX(37)-JVS(273)*XX(43)-JVS(388)*XX(50)-JVS(420)*XX(51)&
             &-JVS(522)*XX(55)
  XX(22) = XX(22)-JVS(387)*XX(50)-JVS(439)*XX(52)-JVS(553)*XX(56)
  XX(21) = XX(21)-JVS(236)*XX(40)-JVS(272)*XX(43)-JVS(386)*XX(50)-JVS(521)*XX(55)
  XX(20) = XX(20)-JVS(180)*XX(36)-JVS(293)*XX(44)-JVS(385)*XX(50)-JVS(520)*XX(55)
  XX(19) = XX(19)-JVS(134)*XX(29)
  XX(18) = XX(18)-JVS(150)*XX(30)-JVS(361)*XX(49)-JVS(438)*XX(52)-JVS(519)*XX(55)-JVS(552)*XX(56)
  XX(17) = XX(17)-JVS(235)*XX(40)-JVS(384)*XX(50)
  XX(16) = XX(16)-JVS(383)*XX(50)-JVS(437)*XX(52)-JVS(518)*XX(55)
  XX(15) = XX(15)-JVS(360)*XX(49)-JVS(436)*XX(52)
  XX(14) = XX(14)-JVS(292)*XX(44)-JVS(382)*XX(50)
  XX(13) = XX(13)-JVS(58)*XX(18)-JVS(103)*XX(26)-JVS(133)*XX(29)-JVS(200)*XX(37)-JVS(257)*XX(42)-JVS(381)*XX(50)&
             &-JVS(517)*XX(55)
  XX(12) = XX(12)-JVS(107)*XX(27)-JVS(435)*XX(52)-JVS(487)*XX(54)
  XX(11) = XX(11)-JVS(57)*XX(18)-JVS(102)*XX(26)-JVS(199)*XX(37)-JVS(380)*XX(50)-JVS(516)*XX(55)
  XX(10) = XX(10)-JVS(434)*XX(52)-JVS(467)*XX(53)
  XX(9) = XX(9)-JVS(379)*XX(50)-JVS(515)*XX(55)
  XX(8) = XX(8)-JVS(271)*XX(43)-JVS(378)*XX(50)-JVS(514)*XX(55)
  XX(7) = XX(7)-JVS(377)*XX(50)-JVS(513)*XX(55)
  XX(6) = XX(6)-JVS(96)*XX(25)-JVS(376)*XX(50)
  XX(5) = XX(5)
  XX(4) = XX(4)
  XX(3) = XX(3)
  XX(2) = XX(2)
  XX(1) = XX(1)
      
END SUBROUTINE KppSolveTR

! End of KppSolveTR function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE cbmz_bb_LinearAlgebra

