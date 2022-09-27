! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! The ODE Jacobian of Chemical Model File
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
! File                 : racm_mim_Jacobian.f90
! Time                 : Mon Sep 26 23:37:27 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/racm_mim
! Equation file        : racm_mim.kpp
! Output root filename : racm_mim
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE racm_mim_Jacobian

  USE racm_mim_Parameters
  USE racm_mim_JacobianSP

  IMPLICIT NONE

CONTAINS


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Jac_SP_Vec - function for sparse multiplication: sparse Jacobian times vector
!   Arguments :
!      JVS       - sparse Jacobian of variables
!      UV        - User vector for variables
!      JUV       - Jacobian times user vector
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Jac_SP_Vec ( JVS, UV, JUV )

! JVS - sparse Jacobian of variables
  REAL(kind=dp) :: JVS(LU_NONZERO)
! UV - User vector for variables
  REAL(kind=dp) :: UV(NVAR)
! JUV - Jacobian times user vector
  REAL(kind=dp) :: JUV(NVAR)

  JUV(1) = JVS(1)*UV(1)+JVS(2)*UV(5)+JVS(3)*UV(73)
  JUV(2) = JVS(4)*UV(2)+JVS(5)*UV(37)+JVS(6)*UV(73)
  JUV(3) = JVS(7)*UV(3)+JVS(8)*UV(14)+JVS(9)*UV(30)+JVS(10)*UV(38)+JVS(11)*UV(39)+JVS(12)*UV(41)+JVS(13)*UV(43)+JVS(14)&
             &*UV(48)+JVS(15)*UV(61)+JVS(16)*UV(63)+JVS(17)*UV(73)+JVS(18)*UV(77)
  JUV(4) = JVS(19)*UV(4)+JVS(20)*UV(38)+JVS(21)*UV(45)+JVS(22)*UV(46)+JVS(23)*UV(47)+JVS(24)*UV(50)+JVS(25)*UV(53)&
             &+JVS(26)*UV(55)+JVS(27)*UV(61)+JVS(28)*UV(63)+JVS(29)*UV(65)+JVS(30)*UV(66)+JVS(31)*UV(67)+JVS(32)*UV(68)&
             &+JVS(33)*UV(69)+JVS(34)*UV(71)+JVS(35)*UV(72)+JVS(36)*UV(75)+JVS(37)*UV(76)+JVS(38)*UV(77)+JVS(39)*UV(78)
  JUV(5) = JVS(40)*UV(5)+JVS(41)*UV(73)
  JUV(6) = JVS(42)*UV(6)+JVS(43)*UV(77)
  JUV(7) = JVS(44)*UV(7)+JVS(45)*UV(50)+JVS(46)*UV(73)+JVS(47)*UV(75)
  JUV(8) = JVS(48)*UV(8)+JVS(49)*UV(73)
  JUV(9) = JVS(50)*UV(9)+JVS(51)*UV(73)
  JUV(10) = JVS(52)*UV(10)+JVS(53)*UV(73)
  JUV(11) = JVS(54)*UV(11)+JVS(55)*UV(74)+JVS(56)*UV(80)
  JUV(12) = JVS(57)*UV(12)+JVS(58)*UV(73)
  JUV(13) = JVS(59)*UV(13)+JVS(60)*UV(42)+JVS(61)*UV(73)+JVS(62)*UV(75)
  JUV(14) = JVS(63)*UV(14)+JVS(64)*UV(73)
  JUV(15) = JVS(65)*UV(15)+JVS(66)*UV(63)+JVS(67)*UV(65)+JVS(68)*UV(73)+JVS(69)*UV(77)
  JUV(16) = JVS(70)*UV(16)+JVS(71)*UV(61)+JVS(72)*UV(73)
  JUV(17) = JVS(73)*UV(17)+JVS(74)*UV(63)+JVS(75)*UV(65)+JVS(76)*UV(73)+JVS(77)*UV(77)
  JUV(18) = JVS(78)*UV(18)+JVS(79)*UV(34)+JVS(80)*UV(73)
  JUV(19) = JVS(81)*UV(19)+JVS(82)*UV(73)+JVS(83)*UV(75)+JVS(84)*UV(80)
  JUV(20) = JVS(85)*UV(20)+JVS(86)*UV(73)+JVS(87)*UV(75)+JVS(88)*UV(76)
  JUV(21) = JVS(89)*UV(21)+JVS(90)*UV(42)+JVS(91)*UV(73)+JVS(92)*UV(80)
  JUV(22) = JVS(93)*UV(21)+JVS(94)*UV(22)+JVS(95)*UV(34)+JVS(96)*UV(42)+JVS(97)*UV(73)+JVS(98)*UV(79)
  JUV(23) = JVS(100)*UV(23)+JVS(101)*UV(28)+JVS(102)*UV(29)+JVS(103)*UV(31)+JVS(104)*UV(73)+JVS(105)*UV(79)+JVS(106)&
              &*UV(80)
  JUV(24) = JVS(107)*UV(24)+JVS(108)*UV(36)+JVS(109)*UV(38)+JVS(110)*UV(39)+JVS(111)*UV(41)+JVS(112)*UV(63)+JVS(113)&
              &*UV(65)+JVS(114)*UV(73)+JVS(115)*UV(75)+JVS(116)*UV(77)
  JUV(25) = JVS(117)*UV(12)+JVS(118)*UV(25)+JVS(119)*UV(43)+JVS(120)*UV(69)+JVS(121)*UV(73)+JVS(122)*UV(76)
  JUV(26) = JVS(123)*UV(6)+JVS(124)*UV(26)+JVS(125)*UV(41)+JVS(126)*UV(74)+JVS(127)*UV(77)+JVS(128)*UV(79)+JVS(129)&
              &*UV(80)
  JUV(27) = JVS(130)*UV(27)+JVS(131)*UV(40)+JVS(132)*UV(73)+JVS(133)*UV(74)+JVS(134)*UV(75)+JVS(135)*UV(80)
  JUV(28) = JVS(136)*UV(9)+JVS(137)*UV(28)+JVS(138)*UV(73)+JVS(139)*UV(77)+JVS(140)*UV(80)
  JUV(29) = JVS(141)*UV(10)+JVS(142)*UV(29)+JVS(143)*UV(73)+JVS(144)*UV(77)+JVS(145)*UV(80)
  JUV(30) = JVS(146)*UV(30)+JVS(147)*UV(73)+JVS(148)*UV(74)+JVS(149)*UV(77)
  JUV(31) = JVS(150)*UV(31)+JVS(151)*UV(40)+JVS(152)*UV(73)+JVS(153)*UV(77)+JVS(154)*UV(80)
  JUV(32) = JVS(155)*UV(32)+JVS(156)*UV(40)+JVS(157)*UV(44)+JVS(158)*UV(51)+JVS(159)*UV(56)+JVS(160)*UV(61)+JVS(161)&
              &*UV(64)+JVS(162)*UV(73)+JVS(163)*UV(74)+JVS(164)*UV(75)+JVS(165)*UV(80)
  JUV(33) = JVS(166)*UV(33)+JVS(167)*UV(61)+JVS(168)*UV(72)+JVS(169)*UV(73)+JVS(170)*UV(75)+JVS(171)*UV(77)
  JUV(34) = JVS(172)*UV(34)+JVS(173)*UV(39)+JVS(174)*UV(50)+JVS(175)*UV(73)+JVS(176)*UV(74)+JVS(177)*UV(79)
  JUV(35) = JVS(178)*UV(35)+JVS(179)*UV(43)+JVS(180)*UV(72)+JVS(181)*UV(73)+JVS(182)*UV(74)+JVS(183)*UV(77)+JVS(184)&
              &*UV(80)
  JUV(36) = JVS(185)*UV(36)+JVS(186)*UV(73)+JVS(187)*UV(74)+JVS(188)*UV(77)
  JUV(37) = JVS(189)*UV(14)+JVS(190)*UV(18)+JVS(191)*UV(30)+JVS(193)*UV(36)+JVS(194)*UV(37)+JVS(195)*UV(38)+JVS(196)&
              &*UV(39)+JVS(197)*UV(41)+JVS(198)*UV(42)+JVS(199)*UV(43)+JVS(200)*UV(44)+JVS(201)*UV(48)+JVS(203)*UV(51)&
              &+JVS(204)*UV(56)+JVS(205)*UV(61)+JVS(206)*UV(63)+JVS(207)*UV(64)+JVS(208)*UV(65)+JVS(209)*UV(73)+JVS(210)&
              &*UV(74)+JVS(211)*UV(77)+JVS(212)*UV(79)
  JUV(38) = JVS(213)*UV(38)+JVS(214)*UV(73)+JVS(215)*UV(74)+JVS(216)*UV(77)
  JUV(39) = JVS(217)*UV(39)+JVS(218)*UV(73)+JVS(219)*UV(74)+JVS(220)*UV(77)
  JUV(40) = JVS(221)*UV(27)+JVS(222)*UV(28)+JVS(223)*UV(29)+JVS(224)*UV(31)+JVS(225)*UV(40)+JVS(226)*UV(73)+JVS(227)&
              &*UV(74)+JVS(228)*UV(75)+JVS(229)*UV(77)+JVS(230)*UV(80)
  JUV(41) = JVS(231)*UV(41)+JVS(232)*UV(73)+JVS(233)*UV(74)+JVS(234)*UV(77)
  JUV(42) = JVS(235)*UV(13)+JVS(236)*UV(21)+JVS(237)*UV(39)+JVS(238)*UV(42)+JVS(239)*UV(48)+JVS(240)*UV(73)+JVS(242)&
              &*UV(75)+JVS(243)*UV(77)+JVS(244)*UV(79)+JVS(245)*UV(80)
  JUV(43) = JVS(246)*UV(43)+JVS(247)*UV(73)+JVS(248)*UV(74)+JVS(249)*UV(77)+JVS(250)*UV(78)+JVS(251)*UV(80)
  JUV(44) = JVS(252)*UV(14)+JVS(253)*UV(44)+JVS(254)*UV(52)+JVS(255)*UV(57)+JVS(256)*UV(58)+JVS(257)*UV(61)+JVS(258)&
              &*UV(71)+JVS(259)*UV(72)+JVS(260)*UV(73)+JVS(261)*UV(74)+JVS(262)*UV(76)+JVS(263)*UV(77)+JVS(264)*UV(79)
  JUV(45) = JVS(265)*UV(30)+JVS(266)*UV(45)+JVS(267)*UV(72)+JVS(268)*UV(73)+JVS(269)*UV(74)+JVS(270)*UV(75)+JVS(271)&
              &*UV(76)+JVS(273)*UV(79)
  JUV(46) = JVS(274)*UV(46)+JVS(275)*UV(63)+JVS(276)*UV(72)+JVS(277)*UV(73)+JVS(278)*UV(74)+JVS(279)*UV(75)+JVS(280)&
              &*UV(76)+JVS(281)*UV(79)
  JUV(47) = JVS(282)*UV(47)+JVS(283)*UV(65)+JVS(284)*UV(72)+JVS(285)*UV(73)+JVS(286)*UV(74)+JVS(287)*UV(75)+JVS(288)&
              &*UV(76)+JVS(289)*UV(79)
  JUV(48) = JVS(290)*UV(7)+JVS(291)*UV(38)+JVS(292)*UV(39)+JVS(293)*UV(41)+JVS(294)*UV(48)+JVS(295)*UV(50)+JVS(296)&
              &*UV(54)+JVS(297)*UV(72)+JVS(298)*UV(73)+JVS(299)*UV(74)+JVS(301)*UV(76)+JVS(302)*UV(77)+JVS(303)*UV(79)
  JUV(49) = JVS(304)*UV(8)+JVS(305)*UV(16)+JVS(306)*UV(36)+JVS(307)*UV(46)+JVS(308)*UV(47)+JVS(309)*UV(49)+JVS(310)&
              &*UV(53)+JVS(311)*UV(55)+JVS(312)*UV(59)+JVS(313)*UV(60)+JVS(314)*UV(61)+JVS(315)*UV(63)+JVS(316)*UV(65)&
              &+JVS(317)*UV(66)+JVS(318)*UV(67)+JVS(319)*UV(69)+JVS(320)*UV(70)+JVS(321)*UV(71)+JVS(322)*UV(72)+JVS(323)&
              &*UV(73)+JVS(324)*UV(74)+JVS(326)*UV(76)+JVS(327)*UV(77)+JVS(328)*UV(79)
  JUV(50) = JVS(329)*UV(39)+JVS(330)*UV(41)+JVS(331)*UV(50)+JVS(332)*UV(72)+JVS(333)*UV(73)+JVS(335)*UV(75)+JVS(336)&
              &*UV(76)+JVS(338)*UV(79)
  JUV(51) = JVS(339)*UV(22)+JVS(340)*UV(25)+JVS(343)*UV(42)+JVS(345)*UV(48)+JVS(347)*UV(51)+JVS(348)*UV(52)+JVS(350)&
              &*UV(57)+JVS(351)*UV(58)+JVS(352)*UV(61)+JVS(353)*UV(69)+JVS(354)*UV(71)+JVS(355)*UV(72)+JVS(356)*UV(73)&
              &+JVS(357)*UV(74)+JVS(359)*UV(76)+JVS(360)*UV(77)+JVS(362)*UV(79)
  JUV(52) = JVS(364)*UV(31)+JVS(366)*UV(52)+JVS(367)*UV(72)+JVS(369)*UV(74)+JVS(370)*UV(75)+JVS(371)*UV(76)+JVS(373)&
              &*UV(79)
  JUV(53) = JVS(375)*UV(12)+JVS(376)*UV(53)+JVS(377)*UV(72)+JVS(378)*UV(73)+JVS(379)*UV(74)+JVS(380)*UV(75)+JVS(381)&
              &*UV(76)+JVS(382)*UV(79)
  JUV(54) = JVS(383)*UV(38)+JVS(384)*UV(54)+JVS(385)*UV(72)+JVS(386)*UV(73)+JVS(387)*UV(74)+JVS(388)*UV(75)+JVS(389)&
              &*UV(76)+JVS(391)*UV(79)
  JUV(55) = JVS(392)*UV(8)+JVS(393)*UV(55)+JVS(394)*UV(72)+JVS(395)*UV(73)+JVS(396)*UV(74)+JVS(397)*UV(75)+JVS(398)&
              &*UV(76)+JVS(399)*UV(79)
  JUV(56) = JVS(400)*UV(14)+JVS(401)*UV(18)+JVS(402)*UV(20)+JVS(403)*UV(25)+JVS(404)*UV(30)+JVS(405)*UV(33)+JVS(407)&
              &*UV(35)+JVS(408)*UV(38)+JVS(409)*UV(39)+JVS(410)*UV(41)+JVS(411)*UV(42)+JVS(412)*UV(43)+JVS(413)*UV(44)&
              &+JVS(414)*UV(45)+JVS(415)*UV(46)+JVS(416)*UV(47)+JVS(417)*UV(48)+JVS(418)*UV(50)+JVS(419)*UV(52)+JVS(420)&
              &*UV(53)+JVS(421)*UV(54)+JVS(422)*UV(55)+JVS(423)*UV(56)+JVS(424)*UV(57)+JVS(425)*UV(58)+JVS(426)*UV(59)&
              &+JVS(428)*UV(62)+JVS(429)*UV(63)+JVS(430)*UV(65)+JVS(431)*UV(66)+JVS(432)*UV(67)+JVS(433)*UV(68)+JVS(434)&
              &*UV(69)+JVS(435)*UV(71)+JVS(436)*UV(72)+JVS(437)*UV(73)+JVS(438)*UV(74)+JVS(440)*UV(76)+JVS(441)*UV(77)&
              &+JVS(442)*UV(78)+JVS(443)*UV(79)
  JUV(57) = JVS(445)*UV(28)+JVS(446)*UV(57)+JVS(447)*UV(72)+JVS(449)*UV(74)+JVS(450)*UV(75)+JVS(451)*UV(76)+JVS(453)&
              &*UV(79)
  JUV(58) = JVS(455)*UV(29)+JVS(456)*UV(58)+JVS(457)*UV(72)+JVS(459)*UV(74)+JVS(460)*UV(75)+JVS(461)*UV(76)+JVS(463)&
              &*UV(79)
  JUV(59) = JVS(465)*UV(36)+JVS(466)*UV(59)+JVS(467)*UV(72)+JVS(468)*UV(73)+JVS(469)*UV(74)+JVS(470)*UV(75)+JVS(471)&
              &*UV(76)+JVS(473)*UV(79)
  JUV(60) = JVS(474)*UV(27)+JVS(476)*UV(43)+JVS(477)*UV(53)+JVS(478)*UV(54)+JVS(479)*UV(55)+JVS(480)*UV(57)+JVS(481)&
              &*UV(58)+JVS(482)*UV(59)+JVS(483)*UV(60)+JVS(484)*UV(66)+JVS(485)*UV(67)+JVS(486)*UV(71)+JVS(487)*UV(72)&
              &+JVS(488)*UV(73)+JVS(489)*UV(74)+JVS(490)*UV(75)+JVS(491)*UV(76)+JVS(494)*UV(79)+JVS(495)*UV(80)
  JUV(61) = JVS(496)*UV(57)+JVS(497)*UV(58)+JVS(498)*UV(61)+JVS(499)*UV(72)+JVS(500)*UV(73)+JVS(501)*UV(74)+JVS(503)&
              &*UV(76)+JVS(504)*UV(77)+JVS(505)*UV(79)
  JUV(62) = JVS(507)*UV(9)+JVS(508)*UV(10)+JVS(509)*UV(33)+JVS(510)*UV(35)+JVS(511)*UV(40)+JVS(512)*UV(41)+JVS(513)&
              &*UV(43)+JVS(514)*UV(53)+JVS(515)*UV(55)+JVS(516)*UV(61)+JVS(517)*UV(62)+JVS(518)*UV(69)+JVS(519)*UV(70)&
              &+JVS(520)*UV(71)+JVS(521)*UV(72)+JVS(522)*UV(73)+JVS(523)*UV(74)+JVS(524)*UV(75)+JVS(525)*UV(76)+JVS(526)&
              &*UV(77)+JVS(528)*UV(79)
  JUV(63) = JVS(530)*UV(38)+JVS(531)*UV(41)+JVS(532)*UV(50)+JVS(533)*UV(63)+JVS(534)*UV(72)+JVS(535)*UV(73)+JVS(536)&
              &*UV(74)+JVS(538)*UV(76)+JVS(539)*UV(77)
  JUV(64) = JVS(541)*UV(12)+JVS(542)*UV(14)+JVS(543)*UV(16)+JVS(544)*UV(36)+JVS(545)*UV(45)+JVS(546)*UV(46)+JVS(547)&
              &*UV(47)+JVS(548)*UV(53)+JVS(549)*UV(55)+JVS(550)*UV(59)+JVS(551)*UV(60)+JVS(552)*UV(61)+JVS(553)*UV(63)&
              &+JVS(554)*UV(64)+JVS(555)*UV(65)+JVS(556)*UV(66)+JVS(557)*UV(67)+JVS(558)*UV(68)+JVS(559)*UV(69)+JVS(560)&
              &*UV(70)+JVS(561)*UV(71)+JVS(562)*UV(72)+JVS(563)*UV(73)+JVS(564)*UV(74)+JVS(566)*UV(76)+JVS(567)*UV(77)&
              &+JVS(569)*UV(79)
  JUV(65) = JVS(571)*UV(50)+JVS(572)*UV(54)+JVS(573)*UV(65)+JVS(574)*UV(72)+JVS(575)*UV(73)+JVS(576)*UV(74)+JVS(578)&
              &*UV(76)+JVS(579)*UV(77)+JVS(580)*UV(79)
  JUV(66) = JVS(581)*UV(30)+JVS(582)*UV(36)+JVS(583)*UV(38)+JVS(584)*UV(41)+JVS(585)*UV(63)+JVS(586)*UV(65)+JVS(587)&
              &*UV(66)+JVS(588)*UV(67)+JVS(589)*UV(72)+JVS(591)*UV(74)+JVS(592)*UV(75)+JVS(593)*UV(76)+JVS(595)*UV(79)
  JUV(67) = JVS(596)*UV(30)+JVS(597)*UV(36)+JVS(598)*UV(38)+JVS(599)*UV(41)+JVS(600)*UV(63)+JVS(601)*UV(65)+JVS(602)&
              &*UV(66)+JVS(603)*UV(67)+JVS(604)*UV(72)+JVS(606)*UV(74)+JVS(607)*UV(75)+JVS(608)*UV(76)+JVS(610)*UV(79)
  JUV(68) = JVS(611)*UV(17)+JVS(612)*UV(36)+JVS(613)*UV(38)+JVS(614)*UV(49)+JVS(615)*UV(53)+JVS(616)*UV(55)+JVS(620)&
              &*UV(63)+JVS(621)*UV(65)+JVS(624)*UV(68)+JVS(627)*UV(71)+JVS(628)*UV(72)+JVS(629)*UV(73)+JVS(630)*UV(74)&
              &+JVS(631)*UV(75)+JVS(632)*UV(76)+JVS(633)*UV(77)+JVS(635)*UV(79)
  JUV(69) = JVS(637)*UV(36)+JVS(638)*UV(38)+JVS(639)*UV(41)+JVS(640)*UV(49)+JVS(646)*UV(63)+JVS(647)*UV(65)+JVS(650)&
              &*UV(69)+JVS(653)*UV(72)+JVS(654)*UV(73)+JVS(655)*UV(74)+JVS(656)*UV(75)+JVS(657)*UV(76)+JVS(658)*UV(77)&
              &+JVS(660)*UV(79)
  JUV(70) = JVS(662)*UV(45)+JVS(663)*UV(46)+JVS(664)*UV(47)+JVS(665)*UV(52)+JVS(666)*UV(53)+JVS(667)*UV(54)+JVS(668)&
              &*UV(55)+JVS(669)*UV(57)+JVS(670)*UV(58)+JVS(671)*UV(59)+JVS(672)*UV(62)+JVS(675)*UV(68)+JVS(676)*UV(69)&
              &+JVS(677)*UV(70)+JVS(678)*UV(71)+JVS(680)*UV(73)+JVS(682)*UV(75)+JVS(685)*UV(78)
  JUV(71) = JVS(688)*UV(14)+JVS(689)*UV(60)+JVS(692)*UV(70)+JVS(693)*UV(71)+JVS(694)*UV(72)+JVS(695)*UV(73)+JVS(696)&
              &*UV(74)+JVS(697)*UV(75)+JVS(698)*UV(76)+JVS(701)*UV(79)
  JUV(72) = JVS(703)*UV(25)+JVS(704)*UV(33)+JVS(705)*UV(35)+JVS(706)*UV(39)+JVS(707)*UV(41)+JVS(708)*UV(42)+JVS(709)&
              &*UV(43)+JVS(710)*UV(45)+JVS(711)*UV(46)+JVS(712)*UV(47)+JVS(713)*UV(48)+JVS(714)*UV(49)+JVS(715)*UV(50)&
              &+JVS(716)*UV(51)+JVS(717)*UV(52)+JVS(718)*UV(53)+JVS(719)*UV(54)+JVS(720)*UV(55)+JVS(721)*UV(57)+JVS(722)&
              &*UV(58)+JVS(723)*UV(59)+JVS(725)*UV(61)+JVS(726)*UV(62)+JVS(728)*UV(64)+JVS(730)*UV(66)+JVS(731)*UV(67)&
              &+JVS(732)*UV(68)+JVS(733)*UV(69)+JVS(735)*UV(71)+JVS(736)*UV(72)+JVS(737)*UV(73)+JVS(738)*UV(74)+JVS(739)&
              &*UV(75)+JVS(740)*UV(76)+JVS(741)*UV(77)+JVS(742)*UV(78)+JVS(743)*UV(79)+JVS(744)*UV(80)
  JUV(73) = JVS(745)*UV(5)+JVS(746)*UV(6)+JVS(747)*UV(8)+JVS(748)*UV(9)+JVS(749)*UV(10)+JVS(750)*UV(12)+JVS(751)*UV(13)&
              &+JVS(752)*UV(14)+JVS(753)*UV(15)+JVS(754)*UV(16)+JVS(755)*UV(17)+JVS(756)*UV(18)+JVS(757)*UV(19)+JVS(758)&
              &*UV(20)+JVS(759)*UV(21)+JVS(760)*UV(22)+JVS(761)*UV(23)+JVS(762)*UV(24)+JVS(763)*UV(25)+JVS(764)*UV(28)&
              &+JVS(765)*UV(29)+JVS(766)*UV(30)+JVS(767)*UV(31)+JVS(768)*UV(32)+JVS(769)*UV(33)+JVS(770)*UV(34)+JVS(771)&
              &*UV(35)+JVS(772)*UV(36)+JVS(773)*UV(37)+JVS(774)*UV(38)+JVS(775)*UV(39)+JVS(776)*UV(40)+JVS(777)*UV(41)&
              &+JVS(779)*UV(43)+JVS(780)*UV(44)+JVS(781)*UV(48)+JVS(782)*UV(49)+JVS(784)*UV(51)+JVS(789)*UV(56)+JVS(793)&
              &*UV(60)+JVS(794)*UV(61)+JVS(796)*UV(63)+JVS(797)*UV(64)+JVS(798)*UV(65)+JVS(803)*UV(70)+JVS(806)*UV(73)&
              &+JVS(807)*UV(74)+JVS(808)*UV(75)+JVS(810)*UV(77)+JVS(812)*UV(79)+JVS(813)*UV(80)
  JUV(74) = JVS(814)*UV(11)+JVS(815)*UV(19)+JVS(816)*UV(26)+JVS(817)*UV(30)+JVS(818)*UV(32)+JVS(819)*UV(35)+JVS(820)&
              &*UV(36)+JVS(821)*UV(38)+JVS(822)*UV(39)+JVS(823)*UV(40)+JVS(824)*UV(41)+JVS(825)*UV(43)+JVS(826)*UV(44)&
              &+JVS(827)*UV(45)+JVS(828)*UV(46)+JVS(829)*UV(47)+JVS(830)*UV(51)+JVS(831)*UV(52)+JVS(832)*UV(53)+JVS(833)&
              &*UV(54)+JVS(834)*UV(55)+JVS(835)*UV(56)+JVS(836)*UV(57)+JVS(837)*UV(58)+JVS(838)*UV(59)+JVS(839)*UV(61)&
              &+JVS(840)*UV(62)+JVS(841)*UV(63)+JVS(842)*UV(64)+JVS(843)*UV(65)+JVS(844)*UV(66)+JVS(845)*UV(67)+JVS(846)&
              &*UV(68)+JVS(847)*UV(69)+JVS(849)*UV(71)+JVS(850)*UV(72)+JVS(851)*UV(73)+JVS(852)*UV(74)+JVS(853)*UV(75)&
              &+JVS(854)*UV(76)+JVS(855)*UV(77)+JVS(856)*UV(78)+JVS(857)*UV(79)+JVS(858)*UV(80)
  JUV(75) = JVS(859)*UV(5)+JVS(860)*UV(8)+JVS(861)*UV(9)+JVS(862)*UV(10)+JVS(863)*UV(12)+JVS(864)*UV(14)+JVS(865)*UV(16)&
              &+JVS(866)*UV(19)+JVS(867)*UV(20)+JVS(868)*UV(22)+JVS(869)*UV(24)+JVS(870)*UV(25)+JVS(871)*UV(27)+JVS(872)&
              &*UV(28)+JVS(873)*UV(29)+JVS(874)*UV(30)+JVS(875)*UV(31)+JVS(876)*UV(33)+JVS(878)*UV(36)+JVS(879)*UV(37)&
              &+JVS(880)*UV(38)+JVS(881)*UV(39)+JVS(882)*UV(40)+JVS(883)*UV(41)+JVS(884)*UV(42)+JVS(885)*UV(43)+JVS(886)&
              &*UV(44)+JVS(887)*UV(45)+JVS(888)*UV(46)+JVS(889)*UV(47)+JVS(890)*UV(48)+JVS(891)*UV(50)+JVS(892)*UV(51)&
              &+JVS(893)*UV(52)+JVS(894)*UV(53)+JVS(895)*UV(54)+JVS(896)*UV(55)+JVS(897)*UV(56)+JVS(898)*UV(57)+JVS(899)&
              &*UV(58)+JVS(900)*UV(59)+JVS(901)*UV(60)+JVS(902)*UV(61)+JVS(903)*UV(62)+JVS(904)*UV(63)+JVS(905)*UV(64)&
              &+JVS(906)*UV(65)+JVS(907)*UV(66)+JVS(908)*UV(67)+JVS(909)*UV(68)+JVS(910)*UV(69)+JVS(911)*UV(70)+JVS(912)&
              &*UV(71)+JVS(913)*UV(72)+JVS(914)*UV(73)+JVS(915)*UV(74)+JVS(916)*UV(75)+JVS(917)*UV(76)+JVS(918)*UV(77)&
              &+JVS(919)*UV(78)+JVS(920)*UV(79)+JVS(921)*UV(80)
  JUV(76) = JVS(922)*UV(15)+JVS(923)*UV(20)+JVS(924)*UV(33)+JVS(925)*UV(39)+JVS(926)*UV(41)+JVS(927)*UV(45)+JVS(928)&
              &*UV(46)+JVS(929)*UV(47)+JVS(930)*UV(50)+JVS(931)*UV(52)+JVS(932)*UV(53)+JVS(933)*UV(54)+JVS(934)*UV(55)&
              &+JVS(935)*UV(57)+JVS(936)*UV(58)+JVS(937)*UV(59)+JVS(939)*UV(62)+JVS(940)*UV(63)+JVS(941)*UV(64)+JVS(942)&
              &*UV(65)+JVS(943)*UV(66)+JVS(944)*UV(67)+JVS(945)*UV(68)+JVS(946)*UV(69)+JVS(948)*UV(71)+JVS(949)*UV(72)&
              &+JVS(950)*UV(73)+JVS(951)*UV(74)+JVS(952)*UV(75)+JVS(953)*UV(76)+JVS(954)*UV(77)+JVS(955)*UV(78)+JVS(956)&
              &*UV(79)
  JUV(77) = JVS(958)*UV(6)+JVS(959)*UV(26)+JVS(960)*UV(28)+JVS(961)*UV(29)+JVS(962)*UV(30)+JVS(963)*UV(31)+JVS(964)&
              &*UV(36)+JVS(965)*UV(38)+JVS(966)*UV(39)+JVS(968)*UV(41)+JVS(969)*UV(43)+JVS(970)*UV(48)+JVS(973)*UV(61)&
              &+JVS(974)*UV(63)+JVS(975)*UV(65)+JVS(976)*UV(72)+JVS(977)*UV(73)+JVS(979)*UV(75)+JVS(981)*UV(77)+JVS(982)&
              &*UV(78)+JVS(983)*UV(79)+JVS(984)*UV(80)
  JUV(78) = JVS(985)*UV(43)+JVS(986)*UV(61)+JVS(987)*UV(72)+JVS(988)*UV(73)+JVS(989)*UV(74)+JVS(990)*UV(75)+JVS(991)&
              &*UV(76)+JVS(993)*UV(78)+JVS(994)*UV(79)+JVS(995)*UV(80)
  JUV(79) = JVS(996)*UV(23)+JVS(997)*UV(26)+JVS(1003)*UV(42)+JVS(1004)*UV(45)+JVS(1005)*UV(46)+JVS(1006)*UV(47)&
              &+JVS(1008)*UV(50)+JVS(1009)*UV(52)+JVS(1010)*UV(53)+JVS(1011)*UV(54)+JVS(1012)*UV(55)+JVS(1013)*UV(57)&
              &+JVS(1014)*UV(58)+JVS(1015)*UV(59)+JVS(1016)*UV(62)+JVS(1019)*UV(66)+JVS(1020)*UV(67)+JVS(1021)*UV(68)&
              &+JVS(1022)*UV(69)+JVS(1024)*UV(71)+JVS(1025)*UV(72)+JVS(1026)*UV(73)+JVS(1027)*UV(74)+JVS(1028)*UV(75)&
              &+JVS(1029)*UV(76)+JVS(1030)*UV(77)+JVS(1031)*UV(78)+JVS(1032)*UV(79)+JVS(1033)*UV(80)
  JUV(80) = JVS(1034)*UV(11)+JVS(1035)*UV(18)+JVS(1036)*UV(19)+JVS(1037)*UV(21)+JVS(1038)*UV(23)+JVS(1039)*UV(26)&
              &+JVS(1040)*UV(27)+JVS(1041)*UV(28)+JVS(1042)*UV(29)+JVS(1043)*UV(31)+JVS(1044)*UV(32)+JVS(1046)*UV(35)&
              &+JVS(1050)*UV(42)+JVS(1051)*UV(43)+JVS(1053)*UV(45)+JVS(1054)*UV(46)+JVS(1055)*UV(47)+JVS(1057)*UV(50)&
              &+JVS(1059)*UV(52)+JVS(1060)*UV(53)+JVS(1061)*UV(54)+JVS(1062)*UV(55)+JVS(1064)*UV(57)+JVS(1065)*UV(58)&
              &+JVS(1066)*UV(59)+JVS(1067)*UV(60)+JVS(1068)*UV(61)+JVS(1069)*UV(62)+JVS(1073)*UV(66)+JVS(1074)*UV(67)&
              &+JVS(1075)*UV(68)+JVS(1076)*UV(69)+JVS(1078)*UV(71)+JVS(1079)*UV(72)+JVS(1080)*UV(73)+JVS(1081)*UV(74)&
              &+JVS(1082)*UV(75)+JVS(1083)*UV(76)+JVS(1084)*UV(77)+JVS(1085)*UV(78)+JVS(1086)*UV(79)+JVS(1087)*UV(80)
      
END SUBROUTINE Jac_SP_Vec

! End of Jac_SP_Vec function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! JacTR_SP_Vec - sparse multiplication: sparse Jacobian transposed times vector
!   Arguments :
!      JVS       - sparse Jacobian of variables
!      UV        - User vector for variables
!      JTUV      - Jacobian transposed times user vector
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE JacTR_SP_Vec ( JVS, UV, JTUV )

! JVS - sparse Jacobian of variables
  REAL(kind=dp) :: JVS(LU_NONZERO)
! UV - User vector for variables
  REAL(kind=dp) :: UV(NVAR)
! JTUV - Jacobian transposed times user vector
  REAL(kind=dp) :: JTUV(NVAR)

  JTUV(1) = JVS(1)*UV(1)
  JTUV(2) = JVS(4)*UV(2)
  JTUV(3) = JVS(7)*UV(3)
  JTUV(4) = JVS(19)*UV(4)
  JTUV(5) = JVS(2)*UV(1)+JVS(40)*UV(5)+JVS(745)*UV(73)+JVS(859)*UV(75)
  JTUV(6) = JVS(42)*UV(6)+JVS(123)*UV(26)+JVS(746)*UV(73)+JVS(958)*UV(77)
  JTUV(7) = JVS(44)*UV(7)+JVS(290)*UV(48)
  JTUV(8) = JVS(48)*UV(8)+JVS(304)*UV(49)+JVS(392)*UV(55)+JVS(747)*UV(73)+JVS(860)*UV(75)
  JTUV(9) = JVS(50)*UV(9)+JVS(136)*UV(28)+JVS(507)*UV(62)+JVS(748)*UV(73)+JVS(861)*UV(75)
  JTUV(10) = JVS(52)*UV(10)+JVS(141)*UV(29)+JVS(508)*UV(62)+JVS(749)*UV(73)+JVS(862)*UV(75)
  JTUV(11) = JVS(54)*UV(11)+JVS(814)*UV(74)+JVS(1034)*UV(80)
  JTUV(12) = JVS(57)*UV(12)+JVS(117)*UV(25)+JVS(375)*UV(53)+JVS(541)*UV(64)+JVS(750)*UV(73)+JVS(863)*UV(75)
  JTUV(13) = JVS(59)*UV(13)+JVS(235)*UV(42)+JVS(751)*UV(73)
  JTUV(14) = JVS(8)*UV(3)+JVS(63)*UV(14)+JVS(189)*UV(37)+JVS(252)*UV(44)+JVS(400)*UV(56)+JVS(542)*UV(64)+JVS(688)*UV(71)&
               &+JVS(752)*UV(73)+JVS(864)*UV(75)
  JTUV(15) = JVS(65)*UV(15)+JVS(753)*UV(73)+JVS(922)*UV(76)
  JTUV(16) = JVS(70)*UV(16)+JVS(305)*UV(49)+JVS(543)*UV(64)+JVS(754)*UV(73)+JVS(865)*UV(75)
  JTUV(17) = JVS(73)*UV(17)+JVS(611)*UV(68)+JVS(755)*UV(73)
  JTUV(18) = JVS(78)*UV(18)+JVS(190)*UV(37)+JVS(401)*UV(56)+JVS(756)*UV(73)+JVS(1035)*UV(80)
  JTUV(19) = JVS(81)*UV(19)+JVS(757)*UV(73)+JVS(815)*UV(74)+JVS(866)*UV(75)+JVS(1036)*UV(80)
  JTUV(20) = JVS(85)*UV(20)+JVS(402)*UV(56)+JVS(758)*UV(73)+JVS(867)*UV(75)+JVS(923)*UV(76)
  JTUV(21) = JVS(89)*UV(21)+JVS(93)*UV(22)+JVS(236)*UV(42)+JVS(759)*UV(73)+JVS(1037)*UV(80)
  JTUV(22) = JVS(94)*UV(22)+JVS(339)*UV(51)+JVS(760)*UV(73)+JVS(868)*UV(75)
  JTUV(23) = JVS(100)*UV(23)+JVS(761)*UV(73)+JVS(996)*UV(79)+JVS(1038)*UV(80)
  JTUV(24) = JVS(107)*UV(24)+JVS(762)*UV(73)+JVS(869)*UV(75)
  JTUV(25) = JVS(118)*UV(25)+JVS(340)*UV(51)+JVS(403)*UV(56)+JVS(703)*UV(72)+JVS(763)*UV(73)+JVS(870)*UV(75)
  JTUV(26) = JVS(124)*UV(26)+JVS(816)*UV(74)+JVS(959)*UV(77)+JVS(997)*UV(79)+JVS(1039)*UV(80)
  JTUV(27) = JVS(130)*UV(27)+JVS(221)*UV(40)+JVS(474)*UV(60)+JVS(871)*UV(75)+JVS(1040)*UV(80)
  JTUV(28) = JVS(101)*UV(23)+JVS(137)*UV(28)+JVS(222)*UV(40)+JVS(445)*UV(57)+JVS(764)*UV(73)+JVS(872)*UV(75)+JVS(960)&
               &*UV(77)+JVS(1041)*UV(80)
  JTUV(29) = JVS(102)*UV(23)+JVS(142)*UV(29)+JVS(223)*UV(40)+JVS(455)*UV(58)+JVS(765)*UV(73)+JVS(873)*UV(75)+JVS(961)&
               &*UV(77)+JVS(1042)*UV(80)
  JTUV(30) = JVS(9)*UV(3)+JVS(146)*UV(30)+JVS(191)*UV(37)+JVS(265)*UV(45)+JVS(404)*UV(56)+JVS(581)*UV(66)+JVS(596)&
               &*UV(67)+JVS(766)*UV(73)+JVS(817)*UV(74)+JVS(874)*UV(75)+JVS(962)*UV(77)
  JTUV(31) = JVS(103)*UV(23)+JVS(150)*UV(31)+JVS(224)*UV(40)+JVS(364)*UV(52)+JVS(767)*UV(73)+JVS(875)*UV(75)+JVS(963)&
               &*UV(77)+JVS(1043)*UV(80)
  JTUV(32) = JVS(155)*UV(32)+JVS(768)*UV(73)+JVS(818)*UV(74)+JVS(1044)*UV(80)
  JTUV(33) = JVS(166)*UV(33)+JVS(405)*UV(56)+JVS(509)*UV(62)+JVS(704)*UV(72)+JVS(769)*UV(73)+JVS(876)*UV(75)+JVS(924)&
               &*UV(76)
  JTUV(34) = JVS(79)*UV(18)+JVS(95)*UV(22)+JVS(172)*UV(34)+JVS(770)*UV(73)
  JTUV(35) = JVS(178)*UV(35)+JVS(407)*UV(56)+JVS(510)*UV(62)+JVS(705)*UV(72)+JVS(771)*UV(73)+JVS(819)*UV(74)+JVS(1046)&
               &*UV(80)
  JTUV(36) = JVS(108)*UV(24)+JVS(185)*UV(36)+JVS(193)*UV(37)+JVS(306)*UV(49)+JVS(465)*UV(59)+JVS(544)*UV(64)+JVS(582)&
               &*UV(66)+JVS(597)*UV(67)+JVS(612)*UV(68)+JVS(637)*UV(69)+JVS(772)*UV(73)+JVS(820)*UV(74)+JVS(878)*UV(75)&
               &+JVS(964)*UV(77)
  JTUV(37) = JVS(5)*UV(2)+JVS(194)*UV(37)+JVS(773)*UV(73)+JVS(879)*UV(75)
  JTUV(38) = JVS(10)*UV(3)+JVS(20)*UV(4)+JVS(109)*UV(24)+JVS(195)*UV(37)+JVS(213)*UV(38)+JVS(291)*UV(48)+JVS(383)*UV(54)&
               &+JVS(408)*UV(56)+JVS(530)*UV(63)+JVS(583)*UV(66)+JVS(598)*UV(67)+JVS(613)*UV(68)+JVS(638)*UV(69)+JVS(774)&
               &*UV(73)+JVS(821)*UV(74)+JVS(880)*UV(75)+JVS(965)*UV(77)
  JTUV(39) = JVS(11)*UV(3)+JVS(110)*UV(24)+JVS(173)*UV(34)+JVS(196)*UV(37)+JVS(217)*UV(39)+JVS(237)*UV(42)+JVS(292)&
               &*UV(48)+JVS(329)*UV(50)+JVS(409)*UV(56)+JVS(706)*UV(72)+JVS(775)*UV(73)+JVS(822)*UV(74)+JVS(881)*UV(75)&
               &+JVS(925)*UV(76)+JVS(966)*UV(77)
  JTUV(40) = JVS(131)*UV(27)+JVS(151)*UV(31)+JVS(156)*UV(32)+JVS(225)*UV(40)+JVS(511)*UV(62)+JVS(776)*UV(73)+JVS(823)&
               &*UV(74)+JVS(882)*UV(75)
  JTUV(41) = JVS(12)*UV(3)+JVS(111)*UV(24)+JVS(125)*UV(26)+JVS(197)*UV(37)+JVS(231)*UV(41)+JVS(293)*UV(48)+JVS(330)&
               &*UV(50)+JVS(410)*UV(56)+JVS(512)*UV(62)+JVS(531)*UV(63)+JVS(584)*UV(66)+JVS(599)*UV(67)+JVS(639)*UV(69)&
               &+JVS(707)*UV(72)+JVS(777)*UV(73)+JVS(824)*UV(74)+JVS(883)*UV(75)+JVS(926)*UV(76)+JVS(968)*UV(77)
  JTUV(42) = JVS(60)*UV(13)+JVS(90)*UV(21)+JVS(96)*UV(22)+JVS(198)*UV(37)+JVS(238)*UV(42)+JVS(343)*UV(51)+JVS(411)&
               &*UV(56)+JVS(708)*UV(72)+JVS(884)*UV(75)+JVS(1003)*UV(79)+JVS(1050)*UV(80)
  JTUV(43) = JVS(13)*UV(3)+JVS(119)*UV(25)+JVS(179)*UV(35)+JVS(199)*UV(37)+JVS(246)*UV(43)+JVS(412)*UV(56)+JVS(476)&
               &*UV(60)+JVS(513)*UV(62)+JVS(709)*UV(72)+JVS(779)*UV(73)+JVS(825)*UV(74)+JVS(885)*UV(75)+JVS(969)*UV(77)&
               &+JVS(985)*UV(78)+JVS(1051)*UV(80)
  JTUV(44) = JVS(157)*UV(32)+JVS(200)*UV(37)+JVS(253)*UV(44)+JVS(413)*UV(56)+JVS(780)*UV(73)+JVS(826)*UV(74)+JVS(886)&
               &*UV(75)
  JTUV(45) = JVS(21)*UV(4)+JVS(266)*UV(45)+JVS(414)*UV(56)+JVS(545)*UV(64)+JVS(662)*UV(70)+JVS(710)*UV(72)+JVS(827)&
               &*UV(74)+JVS(887)*UV(75)+JVS(927)*UV(76)+JVS(1004)*UV(79)+JVS(1053)*UV(80)
  JTUV(46) = JVS(22)*UV(4)+JVS(274)*UV(46)+JVS(307)*UV(49)+JVS(415)*UV(56)+JVS(546)*UV(64)+JVS(663)*UV(70)+JVS(711)&
               &*UV(72)+JVS(828)*UV(74)+JVS(888)*UV(75)+JVS(928)*UV(76)+JVS(1005)*UV(79)+JVS(1054)*UV(80)
  JTUV(47) = JVS(23)*UV(4)+JVS(282)*UV(47)+JVS(308)*UV(49)+JVS(416)*UV(56)+JVS(547)*UV(64)+JVS(664)*UV(70)+JVS(712)&
               &*UV(72)+JVS(829)*UV(74)+JVS(889)*UV(75)+JVS(929)*UV(76)+JVS(1006)*UV(79)+JVS(1055)*UV(80)
  JTUV(48) = JVS(14)*UV(3)+JVS(201)*UV(37)+JVS(239)*UV(42)+JVS(294)*UV(48)+JVS(345)*UV(51)+JVS(417)*UV(56)+JVS(713)&
               &*UV(72)+JVS(781)*UV(73)+JVS(890)*UV(75)+JVS(970)*UV(77)
  JTUV(49) = JVS(309)*UV(49)+JVS(614)*UV(68)+JVS(640)*UV(69)+JVS(714)*UV(72)+JVS(782)*UV(73)
  JTUV(50) = JVS(24)*UV(4)+JVS(45)*UV(7)+JVS(174)*UV(34)+JVS(295)*UV(48)+JVS(331)*UV(50)+JVS(418)*UV(56)+JVS(532)*UV(63)&
               &+JVS(571)*UV(65)+JVS(715)*UV(72)+JVS(891)*UV(75)+JVS(930)*UV(76)+JVS(1008)*UV(79)+JVS(1057)*UV(80)
  JTUV(51) = JVS(158)*UV(32)+JVS(203)*UV(37)+JVS(347)*UV(51)+JVS(716)*UV(72)+JVS(784)*UV(73)+JVS(830)*UV(74)+JVS(892)&
               &*UV(75)
  JTUV(52) = JVS(254)*UV(44)+JVS(348)*UV(51)+JVS(366)*UV(52)+JVS(419)*UV(56)+JVS(665)*UV(70)+JVS(717)*UV(72)+JVS(831)&
               &*UV(74)+JVS(893)*UV(75)+JVS(931)*UV(76)+JVS(1009)*UV(79)+JVS(1059)*UV(80)
  JTUV(53) = JVS(25)*UV(4)+JVS(310)*UV(49)+JVS(376)*UV(53)+JVS(420)*UV(56)+JVS(477)*UV(60)+JVS(514)*UV(62)+JVS(548)&
               &*UV(64)+JVS(615)*UV(68)+JVS(666)*UV(70)+JVS(718)*UV(72)+JVS(832)*UV(74)+JVS(894)*UV(75)+JVS(932)*UV(76)&
               &+JVS(1010)*UV(79)+JVS(1060)*UV(80)
  JTUV(54) = JVS(296)*UV(48)+JVS(384)*UV(54)+JVS(421)*UV(56)+JVS(478)*UV(60)+JVS(572)*UV(65)+JVS(667)*UV(70)+JVS(719)&
               &*UV(72)+JVS(833)*UV(74)+JVS(895)*UV(75)+JVS(933)*UV(76)+JVS(1011)*UV(79)+JVS(1061)*UV(80)
  JTUV(55) = JVS(26)*UV(4)+JVS(311)*UV(49)+JVS(393)*UV(55)+JVS(422)*UV(56)+JVS(479)*UV(60)+JVS(515)*UV(62)+JVS(549)&
               &*UV(64)+JVS(616)*UV(68)+JVS(668)*UV(70)+JVS(720)*UV(72)+JVS(834)*UV(74)+JVS(896)*UV(75)+JVS(934)*UV(76)&
               &+JVS(1012)*UV(79)+JVS(1062)*UV(80)
  JTUV(56) = JVS(159)*UV(32)+JVS(204)*UV(37)+JVS(423)*UV(56)+JVS(789)*UV(73)+JVS(835)*UV(74)+JVS(897)*UV(75)
  JTUV(57) = JVS(255)*UV(44)+JVS(350)*UV(51)+JVS(424)*UV(56)+JVS(446)*UV(57)+JVS(480)*UV(60)+JVS(496)*UV(61)+JVS(669)&
               &*UV(70)+JVS(721)*UV(72)+JVS(836)*UV(74)+JVS(898)*UV(75)+JVS(935)*UV(76)+JVS(1013)*UV(79)+JVS(1064)*UV(80)
  JTUV(58) = JVS(256)*UV(44)+JVS(351)*UV(51)+JVS(425)*UV(56)+JVS(456)*UV(58)+JVS(481)*UV(60)+JVS(497)*UV(61)+JVS(670)&
               &*UV(70)+JVS(722)*UV(72)+JVS(837)*UV(74)+JVS(899)*UV(75)+JVS(936)*UV(76)+JVS(1014)*UV(79)+JVS(1065)*UV(80)
  JTUV(59) = JVS(312)*UV(49)+JVS(426)*UV(56)+JVS(466)*UV(59)+JVS(482)*UV(60)+JVS(550)*UV(64)+JVS(671)*UV(70)+JVS(723)&
               &*UV(72)+JVS(838)*UV(74)+JVS(900)*UV(75)+JVS(937)*UV(76)+JVS(1015)*UV(79)+JVS(1066)*UV(80)
  JTUV(60) = JVS(313)*UV(49)+JVS(483)*UV(60)+JVS(551)*UV(64)+JVS(689)*UV(71)+JVS(793)*UV(73)+JVS(901)*UV(75)+JVS(1067)&
               &*UV(80)
  JTUV(61) = JVS(15)*UV(3)+JVS(27)*UV(4)+JVS(71)*UV(16)+JVS(160)*UV(32)+JVS(167)*UV(33)+JVS(205)*UV(37)+JVS(257)*UV(44)&
               &+JVS(314)*UV(49)+JVS(352)*UV(51)+JVS(498)*UV(61)+JVS(516)*UV(62)+JVS(552)*UV(64)+JVS(725)*UV(72)+JVS(794)&
               &*UV(73)+JVS(839)*UV(74)+JVS(902)*UV(75)+JVS(973)*UV(77)+JVS(986)*UV(78)+JVS(1068)*UV(80)
  JTUV(62) = JVS(428)*UV(56)+JVS(517)*UV(62)+JVS(672)*UV(70)+JVS(726)*UV(72)+JVS(840)*UV(74)+JVS(903)*UV(75)+JVS(939)&
               &*UV(76)+JVS(1016)*UV(79)+JVS(1069)*UV(80)
  JTUV(63) = JVS(16)*UV(3)+JVS(28)*UV(4)+JVS(66)*UV(15)+JVS(74)*UV(17)+JVS(112)*UV(24)+JVS(206)*UV(37)+JVS(275)*UV(46)&
               &+JVS(315)*UV(49)+JVS(429)*UV(56)+JVS(533)*UV(63)+JVS(553)*UV(64)+JVS(585)*UV(66)+JVS(600)*UV(67)+JVS(620)&
               &*UV(68)+JVS(646)*UV(69)+JVS(796)*UV(73)+JVS(841)*UV(74)+JVS(904)*UV(75)+JVS(940)*UV(76)+JVS(974)*UV(77)
  JTUV(64) = JVS(161)*UV(32)+JVS(207)*UV(37)+JVS(554)*UV(64)+JVS(728)*UV(72)+JVS(797)*UV(73)+JVS(842)*UV(74)+JVS(905)&
               &*UV(75)+JVS(941)*UV(76)
  JTUV(65) = JVS(29)*UV(4)+JVS(67)*UV(15)+JVS(75)*UV(17)+JVS(113)*UV(24)+JVS(208)*UV(37)+JVS(283)*UV(47)+JVS(316)*UV(49)&
               &+JVS(430)*UV(56)+JVS(555)*UV(64)+JVS(573)*UV(65)+JVS(586)*UV(66)+JVS(601)*UV(67)+JVS(621)*UV(68)+JVS(647)&
               &*UV(69)+JVS(798)*UV(73)+JVS(843)*UV(74)+JVS(906)*UV(75)+JVS(942)*UV(76)+JVS(975)*UV(77)
  JTUV(66) = JVS(30)*UV(4)+JVS(317)*UV(49)+JVS(431)*UV(56)+JVS(484)*UV(60)+JVS(556)*UV(64)+JVS(587)*UV(66)+JVS(602)&
               &*UV(67)+JVS(730)*UV(72)+JVS(844)*UV(74)+JVS(907)*UV(75)+JVS(943)*UV(76)+JVS(1019)*UV(79)+JVS(1073)*UV(80)
  JTUV(67) = JVS(31)*UV(4)+JVS(318)*UV(49)+JVS(432)*UV(56)+JVS(485)*UV(60)+JVS(557)*UV(64)+JVS(588)*UV(66)+JVS(603)&
               &*UV(67)+JVS(731)*UV(72)+JVS(845)*UV(74)+JVS(908)*UV(75)+JVS(944)*UV(76)+JVS(1020)*UV(79)+JVS(1074)*UV(80)
  JTUV(68) = JVS(32)*UV(4)+JVS(433)*UV(56)+JVS(558)*UV(64)+JVS(624)*UV(68)+JVS(675)*UV(70)+JVS(732)*UV(72)+JVS(846)&
               &*UV(74)+JVS(909)*UV(75)+JVS(945)*UV(76)+JVS(1021)*UV(79)+JVS(1075)*UV(80)
  JTUV(69) = JVS(33)*UV(4)+JVS(120)*UV(25)+JVS(319)*UV(49)+JVS(353)*UV(51)+JVS(434)*UV(56)+JVS(518)*UV(62)+JVS(559)&
               &*UV(64)+JVS(650)*UV(69)+JVS(676)*UV(70)+JVS(733)*UV(72)+JVS(847)*UV(74)+JVS(910)*UV(75)+JVS(946)*UV(76)&
               &+JVS(1022)*UV(79)+JVS(1076)*UV(80)
  JTUV(70) = JVS(320)*UV(49)+JVS(519)*UV(62)+JVS(560)*UV(64)+JVS(677)*UV(70)+JVS(692)*UV(71)+JVS(803)*UV(73)+JVS(911)&
               &*UV(75)
  JTUV(71) = JVS(34)*UV(4)+JVS(258)*UV(44)+JVS(321)*UV(49)+JVS(354)*UV(51)+JVS(435)*UV(56)+JVS(486)*UV(60)+JVS(520)&
               &*UV(62)+JVS(561)*UV(64)+JVS(627)*UV(68)+JVS(678)*UV(70)+JVS(693)*UV(71)+JVS(735)*UV(72)+JVS(849)*UV(74)&
               &+JVS(912)*UV(75)+JVS(948)*UV(76)+JVS(1024)*UV(79)+JVS(1078)*UV(80)
  JTUV(72) = JVS(35)*UV(4)+JVS(168)*UV(33)+JVS(180)*UV(35)+JVS(259)*UV(44)+JVS(267)*UV(45)+JVS(276)*UV(46)+JVS(284)&
               &*UV(47)+JVS(297)*UV(48)+JVS(322)*UV(49)+JVS(332)*UV(50)+JVS(355)*UV(51)+JVS(367)*UV(52)+JVS(377)*UV(53)&
               &+JVS(385)*UV(54)+JVS(394)*UV(55)+JVS(436)*UV(56)+JVS(447)*UV(57)+JVS(457)*UV(58)+JVS(467)*UV(59)+JVS(487)&
               &*UV(60)+JVS(499)*UV(61)+JVS(521)*UV(62)+JVS(534)*UV(63)+JVS(562)*UV(64)+JVS(574)*UV(65)+JVS(589)*UV(66)&
               &+JVS(604)*UV(67)+JVS(628)*UV(68)+JVS(653)*UV(69)+JVS(694)*UV(71)+JVS(736)*UV(72)+JVS(850)*UV(74)+JVS(913)&
               &*UV(75)+JVS(949)*UV(76)+JVS(976)*UV(77)+JVS(987)*UV(78)+JVS(1025)*UV(79)+JVS(1079)*UV(80)
  JTUV(73) = JVS(3)*UV(1)+JVS(6)*UV(2)+JVS(17)*UV(3)+JVS(41)*UV(5)+JVS(46)*UV(7)+JVS(49)*UV(8)+JVS(51)*UV(9)+JVS(53)&
               &*UV(10)+JVS(58)*UV(12)+JVS(61)*UV(13)+JVS(64)*UV(14)+JVS(68)*UV(15)+JVS(72)*UV(16)+JVS(76)*UV(17)+JVS(80)&
               &*UV(18)+JVS(82)*UV(19)+JVS(86)*UV(20)+JVS(91)*UV(21)+JVS(97)*UV(22)+JVS(104)*UV(23)+JVS(114)*UV(24)+JVS(121)&
               &*UV(25)+JVS(132)*UV(27)+JVS(138)*UV(28)+JVS(143)*UV(29)+JVS(147)*UV(30)+JVS(152)*UV(31)+JVS(162)*UV(32)&
               &+JVS(169)*UV(33)+JVS(175)*UV(34)+JVS(181)*UV(35)+JVS(186)*UV(36)+JVS(209)*UV(37)+JVS(214)*UV(38)+JVS(218)&
               &*UV(39)+JVS(226)*UV(40)+JVS(232)*UV(41)+JVS(240)*UV(42)+JVS(247)*UV(43)+JVS(260)*UV(44)+JVS(268)*UV(45)&
               &+JVS(277)*UV(46)+JVS(285)*UV(47)+JVS(298)*UV(48)+JVS(323)*UV(49)+JVS(333)*UV(50)+JVS(356)*UV(51)+JVS(378)&
               &*UV(53)+JVS(386)*UV(54)+JVS(395)*UV(55)+JVS(437)*UV(56)+JVS(468)*UV(59)+JVS(488)*UV(60)+JVS(500)*UV(61)&
               &+JVS(522)*UV(62)+JVS(535)*UV(63)+JVS(563)*UV(64)+JVS(575)*UV(65)+JVS(629)*UV(68)+JVS(654)*UV(69)+JVS(680)&
               &*UV(70)+JVS(695)*UV(71)+JVS(737)*UV(72)+JVS(806)*UV(73)+JVS(851)*UV(74)+JVS(914)*UV(75)+JVS(950)*UV(76)&
               &+JVS(977)*UV(77)+JVS(988)*UV(78)+JVS(1026)*UV(79)+JVS(1080)*UV(80)
  JTUV(74) = JVS(55)*UV(11)+JVS(126)*UV(26)+JVS(133)*UV(27)+JVS(148)*UV(30)+JVS(163)*UV(32)+JVS(176)*UV(34)+JVS(182)&
               &*UV(35)+JVS(187)*UV(36)+JVS(210)*UV(37)+JVS(215)*UV(38)+JVS(219)*UV(39)+JVS(227)*UV(40)+JVS(233)*UV(41)&
               &+JVS(248)*UV(43)+JVS(261)*UV(44)+JVS(269)*UV(45)+JVS(278)*UV(46)+JVS(286)*UV(47)+JVS(299)*UV(48)+JVS(324)&
               &*UV(49)+JVS(357)*UV(51)+JVS(369)*UV(52)+JVS(379)*UV(53)+JVS(387)*UV(54)+JVS(396)*UV(55)+JVS(438)*UV(56)&
               &+JVS(449)*UV(57)+JVS(459)*UV(58)+JVS(469)*UV(59)+JVS(489)*UV(60)+JVS(501)*UV(61)+JVS(523)*UV(62)+JVS(536)&
               &*UV(63)+JVS(564)*UV(64)+JVS(576)*UV(65)+JVS(591)*UV(66)+JVS(606)*UV(67)+JVS(630)*UV(68)+JVS(655)*UV(69)&
               &+JVS(696)*UV(71)+JVS(738)*UV(72)+JVS(807)*UV(73)+JVS(852)*UV(74)+JVS(915)*UV(75)+JVS(951)*UV(76)+JVS(989)&
               &*UV(78)+JVS(1027)*UV(79)+JVS(1081)*UV(80)
  JTUV(75) = JVS(36)*UV(4)+JVS(47)*UV(7)+JVS(62)*UV(13)+JVS(83)*UV(19)+JVS(87)*UV(20)+JVS(115)*UV(24)+JVS(134)*UV(27)&
               &+JVS(164)*UV(32)+JVS(170)*UV(33)+JVS(228)*UV(40)+JVS(242)*UV(42)+JVS(270)*UV(45)+JVS(279)*UV(46)+JVS(287)&
               &*UV(47)+JVS(335)*UV(50)+JVS(370)*UV(52)+JVS(380)*UV(53)+JVS(388)*UV(54)+JVS(397)*UV(55)+JVS(450)*UV(57)&
               &+JVS(460)*UV(58)+JVS(470)*UV(59)+JVS(490)*UV(60)+JVS(524)*UV(62)+JVS(592)*UV(66)+JVS(607)*UV(67)+JVS(631)&
               &*UV(68)+JVS(656)*UV(69)+JVS(682)*UV(70)+JVS(697)*UV(71)+JVS(739)*UV(72)+JVS(808)*UV(73)+JVS(853)*UV(74)&
               &+JVS(916)*UV(75)+JVS(952)*UV(76)+JVS(979)*UV(77)+JVS(990)*UV(78)+JVS(1028)*UV(79)+JVS(1082)*UV(80)
  JTUV(76) = JVS(37)*UV(4)+JVS(88)*UV(20)+JVS(122)*UV(25)+JVS(262)*UV(44)+JVS(271)*UV(45)+JVS(280)*UV(46)+JVS(288)&
               &*UV(47)+JVS(301)*UV(48)+JVS(326)*UV(49)+JVS(336)*UV(50)+JVS(359)*UV(51)+JVS(371)*UV(52)+JVS(381)*UV(53)&
               &+JVS(389)*UV(54)+JVS(398)*UV(55)+JVS(440)*UV(56)+JVS(451)*UV(57)+JVS(461)*UV(58)+JVS(471)*UV(59)+JVS(491)&
               &*UV(60)+JVS(503)*UV(61)+JVS(525)*UV(62)+JVS(538)*UV(63)+JVS(566)*UV(64)+JVS(578)*UV(65)+JVS(593)*UV(66)&
               &+JVS(608)*UV(67)+JVS(632)*UV(68)+JVS(657)*UV(69)+JVS(698)*UV(71)+JVS(740)*UV(72)+JVS(854)*UV(74)+JVS(917)&
               &*UV(75)+JVS(953)*UV(76)+JVS(991)*UV(78)+JVS(1029)*UV(79)+JVS(1083)*UV(80)
  JTUV(77) = JVS(18)*UV(3)+JVS(38)*UV(4)+JVS(43)*UV(6)+JVS(69)*UV(15)+JVS(77)*UV(17)+JVS(116)*UV(24)+JVS(127)*UV(26)&
               &+JVS(139)*UV(28)+JVS(144)*UV(29)+JVS(149)*UV(30)+JVS(153)*UV(31)+JVS(171)*UV(33)+JVS(183)*UV(35)+JVS(188)&
               &*UV(36)+JVS(211)*UV(37)+JVS(216)*UV(38)+JVS(220)*UV(39)+JVS(229)*UV(40)+JVS(234)*UV(41)+JVS(243)*UV(42)&
               &+JVS(249)*UV(43)+JVS(263)*UV(44)+JVS(302)*UV(48)+JVS(327)*UV(49)+JVS(360)*UV(51)+JVS(441)*UV(56)+JVS(504)&
               &*UV(61)+JVS(526)*UV(62)+JVS(539)*UV(63)+JVS(567)*UV(64)+JVS(579)*UV(65)+JVS(633)*UV(68)+JVS(658)*UV(69)&
               &+JVS(741)*UV(72)+JVS(810)*UV(73)+JVS(855)*UV(74)+JVS(918)*UV(75)+JVS(954)*UV(76)+JVS(981)*UV(77)+JVS(1030)&
               &*UV(79)+JVS(1084)*UV(80)
  JTUV(78) = JVS(39)*UV(4)+JVS(250)*UV(43)+JVS(442)*UV(56)+JVS(685)*UV(70)+JVS(742)*UV(72)+JVS(856)*UV(74)+JVS(919)&
               &*UV(75)+JVS(955)*UV(76)+JVS(982)*UV(77)+JVS(993)*UV(78)+JVS(1031)*UV(79)+JVS(1085)*UV(80)
  JTUV(79) = JVS(98)*UV(22)+JVS(105)*UV(23)+JVS(128)*UV(26)+JVS(177)*UV(34)+JVS(212)*UV(37)+JVS(244)*UV(42)+JVS(264)&
               &*UV(44)+JVS(273)*UV(45)+JVS(281)*UV(46)+JVS(289)*UV(47)+JVS(303)*UV(48)+JVS(328)*UV(49)+JVS(338)*UV(50)&
               &+JVS(362)*UV(51)+JVS(373)*UV(52)+JVS(382)*UV(53)+JVS(391)*UV(54)+JVS(399)*UV(55)+JVS(443)*UV(56)+JVS(453)&
               &*UV(57)+JVS(463)*UV(58)+JVS(473)*UV(59)+JVS(494)*UV(60)+JVS(505)*UV(61)+JVS(528)*UV(62)+JVS(569)*UV(64)&
               &+JVS(580)*UV(65)+JVS(595)*UV(66)+JVS(610)*UV(67)+JVS(635)*UV(68)+JVS(660)*UV(69)+JVS(701)*UV(71)+JVS(743)&
               &*UV(72)+JVS(812)*UV(73)+JVS(857)*UV(74)+JVS(920)*UV(75)+JVS(956)*UV(76)+JVS(983)*UV(77)+JVS(994)*UV(78)&
               &+JVS(1032)*UV(79)+JVS(1086)*UV(80)
  JTUV(80) = JVS(56)*UV(11)+JVS(84)*UV(19)+JVS(92)*UV(21)+JVS(106)*UV(23)+JVS(129)*UV(26)+JVS(135)*UV(27)+JVS(140)&
               &*UV(28)+JVS(145)*UV(29)+JVS(154)*UV(31)+JVS(165)*UV(32)+JVS(184)*UV(35)+JVS(230)*UV(40)+JVS(245)*UV(42)&
               &+JVS(251)*UV(43)+JVS(495)*UV(60)+JVS(744)*UV(72)+JVS(813)*UV(73)+JVS(858)*UV(74)+JVS(921)*UV(75)+JVS(984)&
               &*UV(77)+JVS(995)*UV(78)+JVS(1033)*UV(79)+JVS(1087)*UV(80)
      
END SUBROUTINE JacTR_SP_Vec

! End of JacTR_SP_Vec function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE racm_mim_Jacobian

