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
! File                 : cb05_sorg_aq_LinearAlgebra.f90
! Time                 : Mon Sep 26 23:36:50 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/cb05_sorg_aq
! Equation file        : cb05_sorg_aq.kpp
! Output root filename : cb05_sorg_aq
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE cb05_sorg_aq_LinearAlgebra

  USE cb05_sorg_aq_Parameters
  USE cb05_sorg_aq_JacobianSP

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
  XX(3) = X(3)/JVS(7)
  XX(4) = X(4)/JVS(11)
  XX(5) = X(5)/JVS(14)
  XX(6) = X(6)/JVS(17)
  XX(7) = X(7)/JVS(23)
  XX(8) = X(8)/JVS(29)
  XX(9) = X(9)/JVS(32)
  XX(10) = X(10)/JVS(35)
  XX(11) = X(11)/JVS(41)
  XX(12) = (X(12)-JVS(42)*XX(11))/(JVS(44))
  XX(13) = X(13)/JVS(46)
  XX(14) = X(14)/JVS(49)
  XX(15) = (X(15)-JVS(47)*XX(13)-JVS(50)*XX(14))/(JVS(52))
  XX(16) = X(16)/JVS(54)
  XX(17) = X(17)/JVS(57)
  XX(18) = (X(18)-JVS(55)*XX(16)-JVS(58)*XX(17))/(JVS(60))
  XX(19) = X(19)/JVS(62)
  XX(20) = X(20)/JVS(65)
  XX(21) = X(21)/JVS(68)
  XX(22) = X(22)/JVS(71)
  XX(23) = (X(23)-JVS(63)*XX(19)-JVS(66)*XX(20)-JVS(69)*XX(21)-JVS(72)*XX(22))/(JVS(74))
  XX(24) = X(24)/JVS(77)
  XX(25) = X(25)/JVS(80)
  XX(26) = X(26)/JVS(83)
  XX(27) = X(27)/JVS(86)
  XX(28) = X(28)/JVS(89)
  XX(29) = (X(29)-JVS(78)*XX(24)-JVS(81)*XX(25)-JVS(84)*XX(26)-JVS(87)*XX(27)-JVS(90)*XX(28))/(JVS(92))
  XX(30) = X(30)/JVS(96)
  XX(31) = X(31)/JVS(99)
  XX(32) = (X(32)-JVS(97)*XX(30)-JVS(100)*XX(31))/(JVS(102))
  XX(33) = X(33)/JVS(104)
  XX(34) = (X(34)-JVS(105)*XX(33))/(JVS(107))
  XX(35) = X(35)/JVS(109)
  XX(36) = X(36)/JVS(112)
  XX(37) = (X(37)-JVS(110)*XX(35)-JVS(113)*XX(36))/(JVS(115))
  XX(38) = X(38)/JVS(117)
  XX(39) = X(39)/JVS(122)
  XX(40) = (X(40)-JVS(30)*XX(8)-JVS(33)*XX(9))/(JVS(124))
  XX(41) = X(41)/JVS(126)
  XX(42) = X(42)/JVS(129)
  XX(43) = (X(43)-JVS(2)*XX(1)-JVS(5)*XX(2))/(JVS(132))
  XX(44) = X(44)/JVS(134)
  XX(45) = (X(45)-JVS(135)*XX(44))/(JVS(138))
  XX(46) = X(46)/JVS(141)
  XX(47) = (X(47)-JVS(12)*XX(4)-JVS(15)*XX(5))/(JVS(144))
  XX(48) = X(48)/JVS(146)
  XX(49) = (X(49)-JVS(118)*XX(38))/(JVS(149))
  XX(50) = X(50)/JVS(153)
  XX(51) = X(51)/JVS(157)
  XX(52) = X(52)/JVS(165)
  XX(53) = X(53)/JVS(168)
  XX(54) = X(54)/JVS(172)
  XX(55) = X(55)/JVS(177)
  XX(56) = X(56)/JVS(184)
  XX(57) = X(57)/JVS(188)
  XX(58) = X(58)/JVS(191)
  XX(59) = X(59)/JVS(195)
  XX(60) = (X(60)-JVS(119)*XX(38)-JVS(150)*XX(49))/(JVS(199))
  XX(61) = X(61)/JVS(208)
  XX(62) = (X(62)-JVS(158)*XX(51))/(JVS(218))
  XX(63) = X(63)/JVS(222)
  XX(64) = (X(64)-JVS(123)*XX(39)-JVS(130)*XX(42))/(JVS(228))
  XX(65) = X(65)/JVS(233)
  XX(66) = X(66)/JVS(239)
  XX(67) = X(67)/JVS(244)
  XX(68) = X(68)/JVS(252)
  XX(69) = X(69)/JVS(259)
  XX(70) = X(70)/JVS(272)
  XX(71) = (X(71)-JVS(240)*XX(66)-JVS(273)*XX(70))/(JVS(289))
  XX(72) = X(72)/JVS(294)
  XX(73) = (X(73)-JVS(8)*XX(3)-JVS(223)*XX(63)-JVS(260)*XX(69)-JVS(290)*XX(71))/(JVS(303))
  XX(74) = (X(74)-JVS(159)*XX(51)-JVS(245)*XX(67)-JVS(274)*XX(70))/(JVS(309))
  XX(75) = (X(75)-JVS(36)*XX(10)-JVS(275)*XX(70))/(JVS(315))
  XX(76) = (X(76)-JVS(209)*XX(61)-JVS(246)*XX(67)-JVS(276)*XX(70))/(JVS(320))
  XX(77) = (X(77)-JVS(247)*XX(67)-JVS(277)*XX(70))/(JVS(327))
  XX(78) = (X(78)-JVS(234)*XX(65))/(JVS(337))
  XX(79) = (X(79)-JVS(210)*XX(61)-JVS(295)*XX(72)-JVS(338)*XX(78))/(JVS(354))
  XX(80) = (X(80)-JVS(18)*XX(6)-JVS(24)*XX(7)-JVS(211)*XX(61)-JVS(248)*XX(67)-JVS(278)*XX(70)-JVS(339)*XX(78)-JVS(355)&
             &*XX(79))/(JVS(364))
  XX(81) = (X(81)-JVS(241)*XX(66)-JVS(261)*XX(69)-JVS(279)*XX(70)-JVS(356)*XX(79))/(JVS(372))
  XX(82) = (X(82)-JVS(212)*XX(61)-JVS(262)*XX(69)-JVS(280)*XX(70))/(JVS(392))
  XX(83) = (X(83)-JVS(213)*XX(61)-JVS(263)*XX(69)-JVS(281)*XX(70))/(JVS(414))
  XX(84) = (X(84)-JVS(264)*XX(69)-JVS(357)*XX(79)-JVS(393)*XX(82)-JVS(415)*XX(83))/(JVS(437))
  XX(85) = (X(85)-JVS(214)*XX(61)-JVS(219)*XX(62)-JVS(253)*XX(68)-JVS(265)*XX(69)-JVS(282)*XX(70))/(JVS(460))
  XX(86) = (X(86)-JVS(19)*XX(6)-JVS(25)*XX(7)-JVS(37)*XX(10)-JVS(70)*XX(21)-JVS(73)*XX(22)-JVS(75)*XX(23)-JVS(85)*XX(26)&
             &-JVS(88)*XX(27)-JVS(93)*XX(29)-JVS(120)*XX(38)-JVS(139)*XX(45)-JVS(151)*XX(49)-JVS(160)*XX(51)-JVS(200)*XX(60)&
             &-JVS(229)*XX(64)-JVS(242)*XX(66)-JVS(283)*XX(70)-JVS(291)*XX(71)-JVS(310)*XX(74)-JVS(316)*XX(75)-JVS(321)&
             &*XX(76)-JVS(328)*XX(77)-JVS(340)*XX(78)-JVS(358)*XX(79)-JVS(365)*XX(80)-JVS(373)*XX(81)-JVS(394)*XX(82)&
             &-JVS(416)*XX(83)-JVS(438)*XX(84)-JVS(461)*XX(85))/(JVS(483))
  XX(87) = (X(87)-JVS(127)*XX(41)-JVS(142)*XX(46)-JVS(154)*XX(50)-JVS(169)*XX(53)-JVS(185)*XX(56)-JVS(224)*XX(63)&
             &-JVS(266)*XX(69)-JVS(296)*XX(72)-JVS(304)*XX(73)-JVS(341)*XX(78)-JVS(359)*XX(79)-JVS(366)*XX(80)-JVS(374)&
             &*XX(81)-JVS(395)*XX(82)-JVS(417)*XX(83)-JVS(439)*XX(84)-JVS(462)*XX(85)-JVS(484)*XX(86))/(JVS(517))
  XX(88) = (X(88)-JVS(147)*XX(48)-JVS(189)*XX(57)-JVS(192)*XX(58)-JVS(196)*XX(59)-JVS(215)*XX(61)-JVS(230)*XX(64)&
             &-JVS(249)*XX(67)-JVS(284)*XX(70)-JVS(297)*XX(72)-JVS(311)*XX(74)-JVS(322)*XX(76)-JVS(329)*XX(77)-JVS(342)&
             &*XX(78)-JVS(360)*XX(79)-JVS(367)*XX(80)-JVS(375)*XX(81)-JVS(396)*XX(82)-JVS(418)*XX(83)-JVS(440)*XX(84)&
             &-JVS(463)*XX(85)-JVS(485)*XX(86)-JVS(518)*XX(87))/(JVS(549))
  XX(89) = (X(89)-JVS(178)*XX(55)-JVS(235)*XX(65)-JVS(343)*XX(78)-JVS(397)*XX(82)-JVS(419)*XX(83)-JVS(441)*XX(84)&
             &-JVS(464)*XX(85)-JVS(519)*XX(87)-JVS(550)*XX(88))/(JVS(582))
  XX(90) = (X(90)-JVS(131)*XX(42)-JVS(170)*XX(53)-JVS(173)*XX(54)-JVS(179)*XX(55)-JVS(201)*XX(60)-JVS(220)*XX(62)&
             &-JVS(225)*XX(63)-JVS(231)*XX(64)-JVS(236)*XX(65)-JVS(254)*XX(68)-JVS(267)*XX(69)-JVS(305)*XX(73)-JVS(344)&
             &*XX(78)-JVS(398)*XX(82)-JVS(420)*XX(83)-JVS(442)*XX(84)-JVS(465)*XX(85)-JVS(486)*XX(86)-JVS(520)*XX(87)&
             &-JVS(551)*XX(88)-JVS(583)*XX(89))/(JVS(630))
  XX(91) = (X(91)-JVS(128)*XX(41)-JVS(174)*XX(54)-JVS(180)*XX(55)-JVS(421)*XX(83)-JVS(466)*XX(85)-JVS(487)*XX(86)&
             &-JVS(521)*XX(87)-JVS(552)*XX(88)-JVS(584)*XX(89)-JVS(631)*XX(90))/(JVS(651))
  XX(92) = (X(92)-JVS(181)*XX(55)-JVS(193)*XX(58)-JVS(216)*XX(61)-JVS(255)*XX(68)-JVS(422)*XX(83)-JVS(467)*XX(85)&
             &-JVS(522)*XX(87)-JVS(553)*XX(88)-JVS(585)*XX(89)-JVS(632)*XX(90)-JVS(652)*XX(91))/(JVS(672))
  XX(93) = (X(93)-JVS(20)*XX(6)-JVS(26)*XX(7)-JVS(38)*XX(10)-JVS(202)*XX(60)-JVS(285)*XX(70)-JVS(312)*XX(74)-JVS(317)&
             &*XX(75)-JVS(323)*XX(76)-JVS(330)*XX(77)-JVS(345)*XX(78)-JVS(361)*XX(79)-JVS(368)*XX(80)-JVS(376)*XX(81)&
             &-JVS(399)*XX(82)-JVS(423)*XX(83)-JVS(443)*XX(84)-JVS(468)*XX(85)-JVS(488)*XX(86)-JVS(523)*XX(87)-JVS(554)&
             &*XX(88)-JVS(586)*XX(89)-JVS(633)*XX(90)-JVS(653)*XX(91)-JVS(673)*XX(92))/(JVS(696))
  XX(94) = (X(94)-JVS(175)*XX(54)-JVS(182)*XX(55)-JVS(186)*XX(56)-JVS(424)*XX(83)-JVS(469)*XX(85)-JVS(489)*XX(86)&
             &-JVS(524)*XX(87)-JVS(555)*XX(88)-JVS(587)*XX(89)-JVS(634)*XX(90)-JVS(654)*XX(91)-JVS(674)*XX(92)-JVS(697)&
             &*XX(93))/(JVS(715))
  XX(95) = (X(95)-JVS(155)*XX(50)-JVS(161)*XX(51)-JVS(166)*XX(52)-JVS(221)*XX(62)-JVS(232)*XX(64)-JVS(256)*XX(68)&
             &-JVS(292)*XX(71)-JVS(306)*XX(73)-JVS(346)*XX(78)-JVS(400)*XX(82)-JVS(425)*XX(83)-JVS(444)*XX(84)-JVS(470)&
             &*XX(85)-JVS(490)*XX(86)-JVS(525)*XX(87)-JVS(556)*XX(88)-JVS(588)*XX(89)-JVS(635)*XX(90)-JVS(655)*XX(91)&
             &-JVS(675)*XX(92)-JVS(698)*XX(93)-JVS(716)*XX(94))/(JVS(738))
  XX(96) = (X(96)-JVS(3)*XX(1)-JVS(6)*XX(2)-JVS(9)*XX(3)-JVS(13)*XX(4)-JVS(16)*XX(5)-JVS(21)*XX(6)-JVS(27)*XX(7)-JVS(31)&
             &*XX(8)-JVS(34)*XX(9)-JVS(39)*XX(10)-JVS(43)*XX(11)-JVS(45)*XX(12)-JVS(48)*XX(13)-JVS(51)*XX(14)-JVS(53)*XX(15)&
             &-JVS(56)*XX(16)-JVS(59)*XX(17)-JVS(61)*XX(18)-JVS(64)*XX(19)-JVS(67)*XX(20)-JVS(76)*XX(23)-JVS(79)*XX(24)&
             &-JVS(82)*XX(25)-JVS(94)*XX(29)-JVS(98)*XX(30)-JVS(101)*XX(31)-JVS(103)*XX(32)-JVS(106)*XX(33)-JVS(108)*XX(34)&
             &-JVS(111)*XX(35)-JVS(114)*XX(36)-JVS(116)*XX(37)-JVS(121)*XX(38)-JVS(125)*XX(40)-JVS(133)*XX(43)-JVS(136)&
             &*XX(44)-JVS(140)*XX(45)-JVS(145)*XX(47)-JVS(148)*XX(48)-JVS(152)*XX(49)-JVS(156)*XX(50)-JVS(162)*XX(51)&
             &-JVS(167)*XX(52)-JVS(171)*XX(53)-JVS(176)*XX(54)-JVS(183)*XX(55)-JVS(187)*XX(56)-JVS(190)*XX(57)-JVS(194)&
             &*XX(58)-JVS(197)*XX(59)-JVS(203)*XX(60)-JVS(217)*XX(61)-JVS(226)*XX(63)-JVS(237)*XX(65)-JVS(243)*XX(66)&
             &-JVS(250)*XX(67)-JVS(257)*XX(68)-JVS(268)*XX(69)-JVS(286)*XX(70)-JVS(293)*XX(71)-JVS(298)*XX(72)-JVS(307)&
             &*XX(73)-JVS(313)*XX(74)-JVS(318)*XX(75)-JVS(324)*XX(76)-JVS(331)*XX(77)-JVS(347)*XX(78)-JVS(362)*XX(79)&
             &-JVS(369)*XX(80)-JVS(377)*XX(81)-JVS(401)*XX(82)-JVS(426)*XX(83)-JVS(445)*XX(84)-JVS(471)*XX(85)-JVS(491)&
             &*XX(86)-JVS(526)*XX(87)-JVS(557)*XX(88)-JVS(589)*XX(89)-JVS(636)*XX(90)-JVS(656)*XX(91)-JVS(676)*XX(92)&
             &-JVS(699)*XX(93)-JVS(717)*XX(94)-JVS(739)*XX(95))/(JVS(792))
  XX(97) = (X(97)-JVS(10)*XX(3)-JVS(22)*XX(6)-JVS(28)*XX(7)-JVS(40)*XX(10)-JVS(91)*XX(28)-JVS(95)*XX(29)-JVS(143)*XX(46)&
             &-JVS(227)*XX(63)-JVS(269)*XX(69)-JVS(287)*XX(70)-JVS(308)*XX(73)-JVS(314)*XX(74)-JVS(319)*XX(75)-JVS(325)&
             &*XX(76)-JVS(332)*XX(77)-JVS(348)*XX(78)-JVS(363)*XX(79)-JVS(370)*XX(80)-JVS(378)*XX(81)-JVS(402)*XX(82)&
             &-JVS(427)*XX(83)-JVS(446)*XX(84)-JVS(472)*XX(85)-JVS(492)*XX(86)-JVS(527)*XX(87)-JVS(558)*XX(88)-JVS(590)&
             &*XX(89)-JVS(637)*XX(90)-JVS(657)*XX(91)-JVS(677)*XX(92)-JVS(700)*XX(93)-JVS(718)*XX(94)-JVS(740)*XX(95)&
             &-JVS(793)*XX(96))/(JVS(819))
  XX(97) = XX(97)
  XX(96) = XX(96)-JVS(818)*XX(97)
  XX(95) = XX(95)-JVS(791)*XX(96)-JVS(817)*XX(97)
  XX(94) = XX(94)-JVS(737)*XX(95)-JVS(790)*XX(96)-JVS(816)*XX(97)
  XX(93) = XX(93)-JVS(714)*XX(94)-JVS(736)*XX(95)-JVS(789)*XX(96)-JVS(815)*XX(97)
  XX(92) = XX(92)-JVS(695)*XX(93)-JVS(713)*XX(94)-JVS(735)*XX(95)-JVS(788)*XX(96)-JVS(814)*XX(97)
  XX(91) = XX(91)-JVS(671)*XX(92)-JVS(694)*XX(93)-JVS(712)*XX(94)-JVS(734)*XX(95)-JVS(787)*XX(96)-JVS(813)*XX(97)
  XX(90) = XX(90)-JVS(650)*XX(91)-JVS(670)*XX(92)-JVS(693)*XX(93)-JVS(711)*XX(94)-JVS(733)*XX(95)-JVS(786)*XX(96)&
             &-JVS(812)*XX(97)
  XX(89) = XX(89)-JVS(629)*XX(90)-JVS(649)*XX(91)-JVS(669)*XX(92)-JVS(692)*XX(93)-JVS(710)*XX(94)-JVS(732)*XX(95)&
             &-JVS(785)*XX(96)-JVS(811)*XX(97)
  XX(88) = XX(88)-JVS(581)*XX(89)-JVS(628)*XX(90)-JVS(648)*XX(91)-JVS(668)*XX(92)-JVS(691)*XX(93)-JVS(709)*XX(94)&
             &-JVS(731)*XX(95)-JVS(784)*XX(96)-JVS(810)*XX(97)
  XX(87) = XX(87)-JVS(548)*XX(88)-JVS(580)*XX(89)-JVS(627)*XX(90)-JVS(647)*XX(91)-JVS(667)*XX(92)-JVS(690)*XX(93)&
             &-JVS(708)*XX(94)-JVS(730)*XX(95)-JVS(783)*XX(96)-JVS(809)*XX(97)
  XX(86) = XX(86)-JVS(516)*XX(87)-JVS(547)*XX(88)-JVS(579)*XX(89)-JVS(626)*XX(90)-JVS(646)*XX(91)-JVS(666)*XX(92)&
             &-JVS(689)*XX(93)-JVS(707)*XX(94)-JVS(729)*XX(95)-JVS(782)*XX(96)-JVS(808)*XX(97)
  XX(85) = XX(85)-JVS(515)*XX(87)-JVS(546)*XX(88)-JVS(578)*XX(89)-JVS(625)*XX(90)-JVS(665)*XX(92)-JVS(688)*XX(93)&
             &-JVS(728)*XX(95)-JVS(781)*XX(96)-JVS(807)*XX(97)
  XX(84) = XX(84)-JVS(459)*XX(85)-JVS(514)*XX(87)-JVS(545)*XX(88)-JVS(577)*XX(89)-JVS(624)*XX(90)-JVS(645)*XX(91)&
             &-JVS(664)*XX(92)-JVS(687)*XX(93)-JVS(706)*XX(94)-JVS(727)*XX(95)-JVS(780)*XX(96)-JVS(806)*XX(97)
  XX(83) = XX(83)-JVS(513)*XX(87)-JVS(544)*XX(88)-JVS(623)*XX(90)-JVS(644)*XX(91)-JVS(663)*XX(92)-JVS(686)*XX(93)&
             &-JVS(779)*XX(96)-JVS(805)*XX(97)
  XX(82) = XX(82)-JVS(512)*XX(87)-JVS(543)*XX(88)-JVS(622)*XX(90)-JVS(662)*XX(92)-JVS(685)*XX(93)-JVS(705)*XX(94)&
             &-JVS(778)*XX(96)-JVS(804)*XX(97)
  XX(81) = XX(81)-JVS(391)*XX(82)-JVS(413)*XX(83)-JVS(436)*XX(84)-JVS(458)*XX(85)-JVS(482)*XX(86)-JVS(511)*XX(87)&
             &-JVS(542)*XX(88)-JVS(576)*XX(89)-JVS(621)*XX(90)-JVS(643)*XX(91)-JVS(704)*XX(94)-JVS(726)*XX(95)-JVS(777)&
             &*XX(96)-JVS(803)*XX(97)
  XX(80) = XX(80)-JVS(371)*XX(81)-JVS(390)*XX(82)-JVS(412)*XX(83)-JVS(435)*XX(84)-JVS(457)*XX(85)-JVS(481)*XX(86)&
             &-JVS(510)*XX(87)-JVS(541)*XX(88)-JVS(575)*XX(89)-JVS(620)*XX(90)-JVS(684)*XX(93)-JVS(703)*XX(94)-JVS(725)&
             &*XX(95)-JVS(776)*XX(96)-JVS(802)*XX(97)
  XX(79) = XX(79)-JVS(389)*XX(82)-JVS(411)*XX(83)-JVS(434)*XX(84)-JVS(509)*XX(87)-JVS(540)*XX(88)-JVS(574)*XX(89)&
             &-JVS(619)*XX(90)-JVS(724)*XX(95)-JVS(775)*XX(96)
  XX(78) = XX(78)-JVS(388)*XX(82)-JVS(410)*XX(83)-JVS(433)*XX(84)-JVS(573)*XX(89)-JVS(618)*XX(90)-JVS(723)*XX(95)&
             &-JVS(774)*XX(96)
  XX(77) = XX(77)-JVS(336)*XX(78)-JVS(353)*XX(79)-JVS(387)*XX(82)-JVS(409)*XX(83)-JVS(456)*XX(85)-JVS(480)*XX(86)&
             &-JVS(508)*XX(87)-JVS(539)*XX(88)-JVS(572)*XX(89)-JVS(617)*XX(90)-JVS(683)*XX(93)-JVS(773)*XX(96)-JVS(801)&
             &*XX(97)
  XX(76) = XX(76)-JVS(326)*XX(77)-JVS(352)*XX(79)-JVS(386)*XX(82)-JVS(408)*XX(83)-JVS(455)*XX(85)-JVS(479)*XX(86)&
             &-JVS(507)*XX(87)-JVS(538)*XX(88)-JVS(571)*XX(89)-JVS(616)*XX(90)-JVS(682)*XX(93)-JVS(772)*XX(96)-JVS(800)&
             &*XX(97)
  XX(75) = XX(75)-JVS(335)*XX(78)-JVS(351)*XX(79)-JVS(385)*XX(82)-JVS(432)*XX(84)-JVS(454)*XX(85)-JVS(478)*XX(86)&
             &-JVS(506)*XX(87)-JVS(570)*XX(89)-JVS(615)*XX(90)-JVS(681)*XX(93)-JVS(702)*XX(94)-JVS(771)*XX(96)-JVS(799)&
             &*XX(97)
  XX(74) = XX(74)-JVS(384)*XX(82)-JVS(453)*XX(85)-JVS(477)*XX(86)-JVS(505)*XX(87)-JVS(537)*XX(88)-JVS(569)*XX(89)&
             &-JVS(614)*XX(90)-JVS(680)*XX(93)-JVS(770)*XX(96)-JVS(798)*XX(97)
  XX(73) = XX(73)-JVS(383)*XX(82)-JVS(431)*XX(84)-JVS(452)*XX(85)-JVS(476)*XX(86)-JVS(504)*XX(87)-JVS(568)*XX(89)&
             &-JVS(613)*XX(90)-JVS(642)*XX(91)-JVS(769)*XX(96)-JVS(797)*XX(97)
  XX(72) = XX(72)-JVS(334)*XX(78)-JVS(350)*XX(79)-JVS(382)*XX(82)-JVS(407)*XX(83)-JVS(430)*XX(84)-JVS(503)*XX(87)&
             &-JVS(567)*XX(89)-JVS(612)*XX(90)
  XX(71) = XX(71)-JVS(381)*XX(82)-JVS(451)*XX(85)-JVS(475)*XX(86)-JVS(566)*XX(89)-JVS(611)*XX(90)-JVS(641)*XX(91)&
             &-JVS(768)*XX(96)
  XX(70) = XX(70)-JVS(610)*XX(90)-JVS(767)*XX(96)
  XX(69) = XX(69)-JVS(502)*XX(87)-JVS(766)*XX(96)-JVS(796)*XX(97)
  XX(68) = XX(68)-JVS(450)*XX(85)-JVS(565)*XX(89)-JVS(609)*XX(90)-JVS(661)*XX(92)-JVS(765)*XX(96)
  XX(67) = XX(67)-JVS(271)*XX(70)-JVS(536)*XX(88)-JVS(608)*XX(90)-JVS(764)*XX(96)
  XX(66) = XX(66)-JVS(270)*XX(70)-JVS(564)*XX(89)-JVS(607)*XX(90)-JVS(640)*XX(91)-JVS(763)*XX(96)
  XX(65) = XX(65)-JVS(380)*XX(82)-JVS(406)*XX(83)-JVS(563)*XX(89)-JVS(606)*XX(90)-JVS(762)*XX(96)
  XX(64) = XX(64)-JVS(501)*XX(87)-JVS(535)*XX(88)-JVS(605)*XX(90)-JVS(722)*XX(95)-JVS(761)*XX(96)
  XX(63) = XX(63)-JVS(302)*XX(73)-JVS(429)*XX(84)-JVS(500)*XX(87)-JVS(604)*XX(90)
  XX(62) = XX(62)-JVS(251)*XX(68)-JVS(449)*XX(85)-JVS(499)*XX(87)-JVS(603)*XX(90)-JVS(721)*XX(95)-JVS(760)*XX(96)
  XX(61) = XX(61)-JVS(534)*XX(88)-JVS(759)*XX(96)
  XX(60) = XX(60)-JVS(474)*XX(86)-JVS(602)*XX(90)-JVS(679)*XX(93)-JVS(758)*XX(96)
  XX(59) = XX(59)-JVS(207)*XX(61)-JVS(379)*XX(82)-JVS(405)*XX(83)-JVS(448)*XX(85)-JVS(533)*XX(88)-JVS(562)*XX(89)&
             &-JVS(601)*XX(90)-JVS(757)*XX(96)
  XX(58) = XX(58)-JVS(206)*XX(61)-JVS(447)*XX(85)-JVS(532)*XX(88)-JVS(600)*XX(90)-JVS(756)*XX(96)
  XX(57) = XX(57)-JVS(205)*XX(61)-JVS(333)*XX(78)-JVS(404)*XX(83)-JVS(531)*XX(88)-JVS(561)*XX(89)-JVS(599)*XX(90)&
             &-JVS(755)*XX(96)
  XX(56) = XX(56)-JVS(403)*XX(83)-JVS(498)*XX(87)-JVS(701)*XX(94)-JVS(754)*XX(96)
  XX(55) = XX(55)-JVS(660)*XX(92)-JVS(753)*XX(96)
  XX(54) = XX(54)-JVS(639)*XX(91)-JVS(659)*XX(92)-JVS(752)*XX(96)
  XX(53) = XX(53)-JVS(497)*XX(87)-JVS(598)*XX(90)-JVS(751)*XX(96)-JVS(795)*XX(97)
  XX(52) = XX(52)-JVS(288)*XX(71)-JVS(301)*XX(73)-JVS(428)*XX(84)-JVS(496)*XX(87)-JVS(597)*XX(90)-JVS(720)*XX(95)
  XX(51) = XX(51)-JVS(596)*XX(90)-JVS(750)*XX(96)
  XX(50) = XX(50)-JVS(495)*XX(87)-JVS(719)*XX(95)-JVS(749)*XX(96)
  XX(49) = XX(49)-JVS(198)*XX(60)-JVS(473)*XX(86)-JVS(748)*XX(96)
  XX(48) = XX(48)-JVS(204)*XX(61)-JVS(530)*XX(88)-JVS(658)*XX(92)-JVS(747)*XX(96)
  XX(47) = XX(47)-JVS(164)*XX(52)-JVS(238)*XX(66)-JVS(300)*XX(73)-JVS(349)*XX(79)-JVS(560)*XX(89)-JVS(595)*XX(90)&
             &-JVS(746)*XX(96)
  XX(46) = XX(46)-JVS(258)*XX(69)-JVS(494)*XX(87)-JVS(794)*XX(97)
  XX(45) = XX(45)-JVS(594)*XX(90)-JVS(678)*XX(93)-JVS(745)*XX(96)
  XX(44) = XX(44)-JVS(137)*XX(45)-JVS(593)*XX(90)-JVS(744)*XX(96)
  XX(43) = XX(43)-JVS(163)*XX(52)-JVS(299)*XX(73)-JVS(559)*XX(89)-JVS(592)*XX(90)-JVS(743)*XX(96)
  XX(42) = XX(42)-JVS(529)*XX(88)-JVS(742)*XX(96)
  XX(41) = XX(41)-JVS(493)*XX(87)-JVS(638)*XX(91)
  XX(40) = XX(40)-JVS(591)*XX(90)-JVS(741)*XX(96)
  XX(39) = XX(39)-JVS(528)*XX(88)
  XX(38) = XX(38)
  XX(37) = XX(37)
  XX(36) = XX(36)
  XX(35) = XX(35)
  XX(34) = XX(34)
  XX(33) = XX(33)
  XX(32) = XX(32)
  XX(31) = XX(31)
  XX(30) = XX(30)
  XX(29) = XX(29)
  XX(28) = XX(28)
  XX(27) = XX(27)
  XX(26) = XX(26)
  XX(25) = XX(25)
  XX(24) = XX(24)
  XX(23) = XX(23)
  XX(22) = XX(22)
  XX(21) = XX(21)
  XX(20) = XX(20)
  XX(19) = XX(19)
  XX(18) = XX(18)
  XX(17) = XX(17)
  XX(16) = XX(16)
  XX(15) = XX(15)
  XX(14) = XX(14)
  XX(13) = XX(13)
  XX(12) = XX(12)
  XX(11) = XX(11)
  XX(10) = XX(10)
  XX(9) = XX(9)
  XX(8) = XX(8)
  XX(7) = XX(7)
  XX(6) = XX(6)
  XX(5) = XX(5)
  XX(4) = XX(4)
  XX(3) = XX(3)
  XX(2) = XX(2)
  XX(1) = XX(1)
      
END SUBROUTINE KppSolveTR

! End of KppSolveTR function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE cb05_sorg_aq_LinearAlgebra

