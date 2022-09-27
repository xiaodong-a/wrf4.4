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
! File                 : saprc99_Jacobian.f90
! Time                 : Mon Sep 26 23:37:53 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/saprc99
! Equation file        : saprc99.kpp
! Output root filename : saprc99
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE saprc99_Jacobian

  USE saprc99_Parameters
  USE saprc99_JacobianSP

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

  JUV(1) = JVS(1)*UV(1)+JVS(2)*UV(16)+JVS(3)*UV(82)
  JUV(2) = JVS(4)*UV(2)+JVS(5)*UV(36)+JVS(6)*UV(44)+JVS(7)*UV(53)+JVS(8)*UV(55)+JVS(9)*UV(58)+JVS(10)*UV(59)+JVS(11)&
             &*UV(61)+JVS(12)*UV(62)+JVS(13)*UV(63)+JVS(14)*UV(64)+JVS(15)*UV(65)+JVS(16)*UV(66)+JVS(17)*UV(74)+JVS(18)&
             &*UV(78)+JVS(19)*UV(82)
  JUV(3) = JVS(20)*UV(3)+JVS(21)*UV(55)+JVS(22)*UV(63)+JVS(23)*UV(65)+JVS(24)*UV(74)+JVS(25)*UV(76)+JVS(26)*UV(77)&
             &+JVS(27)*UV(81)+JVS(28)*UV(85)+JVS(29)*UV(86)
  JUV(4) = JVS(30)*UV(4)+JVS(31)*UV(58)+JVS(32)*UV(59)+JVS(33)*UV(61)+JVS(34)*UV(63)+JVS(35)*UV(64)+JVS(36)*UV(65)&
             &+JVS(37)*UV(74)+JVS(38)*UV(75)+JVS(39)*UV(77)+JVS(40)*UV(81)+JVS(41)*UV(83)+JVS(42)*UV(84)+JVS(43)*UV(85)&
             &+JVS(44)*UV(86)
  JUV(5) = JVS(45)*UV(5)+JVS(46)*UV(8)+JVS(47)*UV(85)
  JUV(6) = JVS(48)*UV(6)+JVS(49)*UV(7)+JVS(50)*UV(8)+JVS(51)*UV(18)+JVS(52)*UV(82)
  JUV(7) = JVS(53)*UV(7)+JVS(54)*UV(17)+JVS(55)*UV(82)
  JUV(8) = JVS(56)*UV(8)+JVS(57)*UV(17)+JVS(58)*UV(78)+JVS(59)*UV(82)+JVS(60)*UV(85)
  JUV(9) = JVS(61)*UV(9)+JVS(62)*UV(44)+JVS(63)*UV(55)+JVS(64)*UV(74)
  JUV(10) = JVS(65)*UV(10)+JVS(66)*UV(76)+JVS(67)*UV(85)
  JUV(11) = JVS(68)*UV(11)+JVS(69)*UV(75)+JVS(70)*UV(83)+JVS(71)*UV(84)+JVS(72)*UV(85)
  JUV(12) = JVS(73)*UV(12)+JVS(74)*UV(37)+JVS(75)*UV(55)+JVS(76)*UV(79)+JVS(77)*UV(80)
  JUV(13) = JVS(78)*UV(13)+JVS(79)*UV(37)+JVS(80)*UV(47)+JVS(81)*UV(55)+JVS(82)*UV(70)+JVS(83)*UV(74)+JVS(84)*UV(79)&
              &+JVS(85)*UV(80)+JVS(86)*UV(82)
  JUV(14) = JVS(87)*UV(14)+JVS(88)*UV(74)
  JUV(15) = JVS(89)*UV(15)+JVS(90)*UV(82)
  JUV(16) = JVS(91)*UV(16)+JVS(92)*UV(82)
  JUV(17) = JVS(93)*UV(17)+JVS(94)*UV(18)+JVS(95)*UV(82)+JVS(96)*UV(85)
  JUV(18) = JVS(97)*UV(17)+JVS(98)*UV(18)+JVS(99)*UV(58)+JVS(100)*UV(78)+JVS(101)*UV(82)+JVS(102)*UV(85)
  JUV(19) = JVS(103)*UV(19)+JVS(104)*UV(82)
  JUV(20) = JVS(105)*UV(20)+JVS(106)*UV(76)+JVS(107)*UV(79)
  JUV(21) = JVS(108)*UV(21)+JVS(109)*UV(79)+JVS(110)*UV(83)
  JUV(22) = JVS(111)*UV(22)+JVS(112)*UV(79)+JVS(113)*UV(84)
  JUV(23) = JVS(114)*UV(23)+JVS(115)*UV(75)+JVS(116)*UV(79)
  JUV(24) = JVS(117)*UV(24)+JVS(118)*UV(82)+JVS(119)*UV(85)
  JUV(25) = JVS(120)*UV(25)+JVS(121)*UV(82)
  JUV(26) = JVS(122)*UV(26)+JVS(123)*UV(82)
  JUV(27) = JVS(124)*UV(27)+JVS(125)*UV(34)+JVS(126)*UV(59)+JVS(127)*UV(61)+JVS(128)*UV(74)+JVS(129)*UV(82)
  JUV(28) = JVS(130)*UV(28)+JVS(131)*UV(79)+JVS(132)*UV(80)
  JUV(29) = JVS(133)*UV(29)+JVS(134)*UV(78)+JVS(135)*UV(82)
  JUV(30) = JVS(136)*UV(30)+JVS(137)*UV(82)
  JUV(31) = JVS(138)*UV(30)+JVS(139)*UV(31)+JVS(140)*UV(79)+JVS(141)*UV(82)
  JUV(32) = JVS(142)*UV(32)+JVS(143)*UV(82)
  JUV(33) = JVS(144)*UV(33)+JVS(145)*UV(77)+JVS(146)*UV(81)+JVS(147)*UV(82)+JVS(148)*UV(86)
  JUV(34) = JVS(149)*UV(34)+JVS(150)*UV(82)
  JUV(35) = JVS(151)*UV(35)+JVS(152)*UV(81)+JVS(153)*UV(82)+JVS(154)*UV(85)
  JUV(36) = JVS(155)*UV(36)+JVS(156)*UV(68)+JVS(157)*UV(78)+JVS(158)*UV(85)
  JUV(37) = JVS(159)*UV(37)+JVS(160)*UV(46)+JVS(161)*UV(79)+JVS(162)*UV(80)+JVS(163)*UV(85)
  JUV(38) = JVS(164)*UV(38)+JVS(165)*UV(79)+JVS(166)*UV(82)+JVS(167)*UV(85)
  JUV(39) = JVS(168)*UV(39)+JVS(169)*UV(82)
  JUV(40) = JVS(170)*UV(40)+JVS(171)*UV(82)
  JUV(41) = JVS(172)*UV(34)+JVS(173)*UV(40)+JVS(174)*UV(41)+JVS(175)*UV(82)
  JUV(42) = JVS(176)*UV(34)+JVS(177)*UV(40)+JVS(178)*UV(42)+JVS(179)*UV(82)
  JUV(43) = JVS(180)*UV(34)+JVS(181)*UV(40)+JVS(182)*UV(43)+JVS(183)*UV(80)+JVS(184)*UV(82)
  JUV(44) = JVS(185)*UV(44)+JVS(186)*UV(74)+JVS(187)*UV(82)
  JUV(45) = JVS(188)*UV(34)+JVS(189)*UV(40)+JVS(190)*UV(45)+JVS(191)*UV(74)+JVS(192)*UV(82)
  JUV(46) = JVS(193)*UV(37)+JVS(194)*UV(46)+JVS(195)*UV(57)+JVS(196)*UV(79)+JVS(197)*UV(80)+JVS(198)*UV(85)
  JUV(47) = JVS(199)*UV(34)+JVS(200)*UV(40)+JVS(201)*UV(47)+JVS(202)*UV(65)+JVS(203)*UV(74)+JVS(204)*UV(80)+JVS(205)&
              &*UV(82)
  JUV(48) = JVS(206)*UV(48)+JVS(207)*UV(77)+JVS(208)*UV(82)+JVS(209)*UV(85)+JVS(210)*UV(86)
  JUV(49) = JVS(211)*UV(40)+JVS(212)*UV(49)+JVS(213)*UV(57)+JVS(214)*UV(80)+JVS(215)*UV(82)+JVS(216)*UV(85)
  JUV(50) = JVS(217)*UV(34)+JVS(218)*UV(40)+JVS(219)*UV(41)+JVS(220)*UV(42)+JVS(221)*UV(43)+JVS(222)*UV(50)+JVS(223)&
              &*UV(62)+JVS(224)*UV(64)+JVS(225)*UV(66)+JVS(226)*UV(74)+JVS(227)*UV(80)+JVS(228)*UV(82)
  JUV(51) = JVS(229)*UV(39)+JVS(230)*UV(41)+JVS(231)*UV(42)+JVS(232)*UV(44)+JVS(233)*UV(45)+JVS(234)*UV(50)+JVS(235)&
              &*UV(51)+JVS(236)*UV(53)+JVS(237)*UV(55)+JVS(238)*UV(56)+JVS(239)*UV(58)+JVS(240)*UV(59)+JVS(241)*UV(61)&
              &+JVS(242)*UV(62)+JVS(243)*UV(63)+JVS(244)*UV(64)+JVS(245)*UV(65)+JVS(246)*UV(66)+JVS(247)*UV(67)+JVS(248)&
              &*UV(68)+JVS(249)*UV(70)+JVS(250)*UV(71)+JVS(251)*UV(74)+JVS(252)*UV(80)+JVS(253)*UV(82)
  JUV(52) = JVS(254)*UV(28)+JVS(255)*UV(43)+JVS(256)*UV(46)+JVS(257)*UV(47)+JVS(258)*UV(49)+JVS(259)*UV(50)+JVS(260)&
              &*UV(52)+JVS(261)*UV(56)+JVS(263)*UV(62)+JVS(264)*UV(64)+JVS(267)*UV(67)+JVS(268)*UV(68)+JVS(269)*UV(71)&
              &+JVS(271)*UV(79)+JVS(272)*UV(80)+JVS(273)*UV(82)+JVS(274)*UV(85)
  JUV(53) = JVS(275)*UV(53)+JVS(276)*UV(70)+JVS(277)*UV(74)+JVS(278)*UV(80)+JVS(279)*UV(82)
  JUV(54) = JVS(280)*UV(26)+JVS(281)*UV(30)+JVS(282)*UV(31)+JVS(283)*UV(32)+JVS(284)*UV(39)+JVS(285)*UV(54)+JVS(286)&
              &*UV(59)+JVS(287)*UV(61)+JVS(288)*UV(63)+JVS(289)*UV(65)+JVS(290)*UV(69)+JVS(291)*UV(74)+JVS(293)*UV(80)&
              &+JVS(294)*UV(82)
  JUV(55) = JVS(295)*UV(55)+JVS(296)*UV(70)+JVS(297)*UV(74)+JVS(298)*UV(80)+JVS(299)*UV(82)
  JUV(56) = JVS(300)*UV(34)+JVS(301)*UV(40)+JVS(302)*UV(41)+JVS(303)*UV(42)+JVS(304)*UV(44)+JVS(305)*UV(45)+JVS(306)&
              &*UV(49)+JVS(307)*UV(53)+JVS(308)*UV(56)+JVS(310)*UV(59)+JVS(311)*UV(61)+JVS(312)*UV(64)+JVS(313)*UV(70)&
              &+JVS(314)*UV(74)+JVS(315)*UV(80)+JVS(316)*UV(82)
  JUV(57) = JVS(318)*UV(43)+JVS(319)*UV(49)+JVS(320)*UV(57)+JVS(321)*UV(75)+JVS(322)*UV(76)+JVS(323)*UV(78)+JVS(324)&
              &*UV(79)+JVS(325)*UV(80)+JVS(326)*UV(82)+JVS(327)*UV(83)+JVS(328)*UV(84)+JVS(329)*UV(85)
  JUV(58) = JVS(330)*UV(58)+JVS(331)*UV(70)+JVS(332)*UV(74)+JVS(333)*UV(80)+JVS(334)*UV(82)
  JUV(59) = JVS(335)*UV(59)+JVS(336)*UV(70)+JVS(337)*UV(74)+JVS(338)*UV(80)+JVS(339)*UV(82)
  JUV(60) = JVS(340)*UV(30)+JVS(341)*UV(32)+JVS(342)*UV(39)+JVS(343)*UV(41)+JVS(344)*UV(42)+JVS(345)*UV(54)+JVS(346)&
              &*UV(58)+JVS(347)*UV(59)+JVS(348)*UV(60)+JVS(349)*UV(61)+JVS(350)*UV(63)+JVS(351)*UV(65)+JVS(352)*UV(66)&
              &+JVS(353)*UV(69)+JVS(354)*UV(70)+JVS(355)*UV(72)+JVS(356)*UV(73)+JVS(357)*UV(74)+JVS(358)*UV(75)+JVS(359)&
              &*UV(76)+JVS(360)*UV(77)+JVS(361)*UV(78)+JVS(363)*UV(80)+JVS(364)*UV(81)+JVS(365)*UV(82)+JVS(366)*UV(83)&
              &+JVS(367)*UV(84)+JVS(368)*UV(85)+JVS(369)*UV(86)
  JUV(61) = JVS(370)*UV(61)+JVS(371)*UV(70)+JVS(372)*UV(74)+JVS(373)*UV(80)+JVS(374)*UV(82)
  JUV(62) = JVS(375)*UV(58)+JVS(376)*UV(62)+JVS(377)*UV(65)+JVS(378)*UV(70)+JVS(379)*UV(74)+JVS(380)*UV(80)+JVS(381)&
              &*UV(82)
  JUV(63) = JVS(382)*UV(63)+JVS(383)*UV(70)+JVS(384)*UV(74)+JVS(385)*UV(80)+JVS(386)*UV(82)
  JUV(64) = JVS(387)*UV(58)+JVS(388)*UV(64)+JVS(389)*UV(65)+JVS(391)*UV(74)+JVS(392)*UV(80)+JVS(393)*UV(82)
  JUV(65) = JVS(394)*UV(65)+JVS(395)*UV(70)+JVS(396)*UV(74)+JVS(397)*UV(80)+JVS(398)*UV(82)
  JUV(66) = JVS(399)*UV(58)+JVS(400)*UV(65)+JVS(401)*UV(66)+JVS(402)*UV(70)+JVS(403)*UV(74)+JVS(404)*UV(80)+JVS(405)&
              &*UV(82)
  JUV(67) = JVS(406)*UV(19)+JVS(407)*UV(25)+JVS(408)*UV(30)+JVS(409)*UV(32)+JVS(410)*UV(39)+JVS(411)*UV(53)+JVS(412)&
              &*UV(55)+JVS(413)*UV(63)+JVS(414)*UV(64)+JVS(415)*UV(65)+JVS(416)*UV(67)+JVS(417)*UV(69)+JVS(418)*UV(70)&
              &+JVS(419)*UV(71)+JVS(420)*UV(72)+JVS(421)*UV(73)+JVS(422)*UV(74)+JVS(423)*UV(75)+JVS(424)*UV(76)+JVS(425)&
              &*UV(78)+JVS(426)*UV(80)+JVS(427)*UV(82)+JVS(428)*UV(83)+JVS(429)*UV(84)
  JUV(68) = JVS(430)*UV(25)+JVS(431)*UV(30)+JVS(432)*UV(32)+JVS(433)*UV(33)+JVS(434)*UV(35)+JVS(435)*UV(36)+JVS(436)&
              &*UV(39)+JVS(437)*UV(44)+JVS(438)*UV(53)+JVS(439)*UV(54)+JVS(440)*UV(55)+JVS(441)*UV(56)+JVS(443)*UV(58)&
              &+JVS(444)*UV(59)+JVS(445)*UV(61)+JVS(446)*UV(62)+JVS(447)*UV(63)+JVS(448)*UV(64)+JVS(449)*UV(65)+JVS(450)&
              &*UV(66)+JVS(451)*UV(68)+JVS(452)*UV(69)+JVS(453)*UV(70)+JVS(454)*UV(72)+JVS(455)*UV(73)+JVS(456)*UV(74)&
              &+JVS(457)*UV(75)+JVS(458)*UV(76)+JVS(459)*UV(77)+JVS(460)*UV(78)+JVS(462)*UV(80)+JVS(463)*UV(81)+JVS(464)&
              &*UV(82)+JVS(465)*UV(83)+JVS(466)*UV(84)+JVS(467)*UV(85)+JVS(468)*UV(86)
  JUV(69) = JVS(469)*UV(31)+JVS(470)*UV(59)+JVS(471)*UV(61)+JVS(472)*UV(63)+JVS(473)*UV(64)+JVS(474)*UV(65)+JVS(475)&
              &*UV(69)+JVS(478)*UV(78)+JVS(479)*UV(79)+JVS(480)*UV(80)+JVS(481)*UV(82)+JVS(482)*UV(86)
  JUV(70) = JVS(483)*UV(14)+JVS(484)*UV(53)+JVS(485)*UV(55)+JVS(486)*UV(58)+JVS(487)*UV(59)+JVS(488)*UV(61)+JVS(489)&
              &*UV(62)+JVS(490)*UV(63)+JVS(491)*UV(65)+JVS(492)*UV(66)+JVS(493)*UV(70)+JVS(494)*UV(74)+JVS(495)*UV(78)&
              &+JVS(496)*UV(79)+JVS(497)*UV(80)
  JUV(71) = JVS(499)*UV(26)+JVS(500)*UV(30)+JVS(501)*UV(32)+JVS(502)*UV(39)+JVS(503)*UV(41)+JVS(504)*UV(42)+JVS(505)&
              &*UV(45)+JVS(506)*UV(48)+JVS(507)*UV(53)+JVS(508)*UV(55)+JVS(509)*UV(59)+JVS(510)*UV(61)+JVS(511)*UV(62)&
              &+JVS(512)*UV(63)+JVS(513)*UV(64)+JVS(514)*UV(65)+JVS(515)*UV(66)+JVS(516)*UV(69)+JVS(517)*UV(70)+JVS(518)&
              &*UV(71)+JVS(519)*UV(72)+JVS(520)*UV(73)+JVS(521)*UV(74)+JVS(525)*UV(80)+JVS(526)*UV(82)
  JUV(72) = JVS(529)*UV(30)+JVS(530)*UV(32)+JVS(531)*UV(39)+JVS(532)*UV(55)+JVS(533)*UV(62)+JVS(534)*UV(63)+JVS(535)&
              &*UV(64)+JVS(536)*UV(65)+JVS(537)*UV(66)+JVS(538)*UV(69)+JVS(539)*UV(70)+JVS(540)*UV(72)+JVS(541)*UV(73)&
              &+JVS(542)*UV(74)+JVS(543)*UV(77)+JVS(546)*UV(80)+JVS(547)*UV(81)+JVS(548)*UV(82)+JVS(549)*UV(86)
  JUV(73) = JVS(550)*UV(32)+JVS(551)*UV(39)+JVS(552)*UV(40)+JVS(553)*UV(58)+JVS(554)*UV(59)+JVS(555)*UV(61)+JVS(556)&
              &*UV(63)+JVS(557)*UV(64)+JVS(558)*UV(65)+JVS(559)*UV(66)+JVS(560)*UV(69)+JVS(561)*UV(70)+JVS(562)*UV(73)&
              &+JVS(563)*UV(74)+JVS(564)*UV(76)+JVS(565)*UV(77)+JVS(569)*UV(81)+JVS(570)*UV(82)+JVS(571)*UV(83)+JVS(572)&
              &*UV(84)+JVS(573)*UV(86)
  JUV(74) = JVS(574)*UV(44)+JVS(575)*UV(45)+JVS(576)*UV(53)+JVS(577)*UV(55)+JVS(578)*UV(58)+JVS(579)*UV(59)+JVS(580)&
              &*UV(61)+JVS(581)*UV(62)+JVS(582)*UV(63)+JVS(583)*UV(64)+JVS(584)*UV(65)+JVS(585)*UV(66)+JVS(586)*UV(70)&
              &+JVS(587)*UV(74)+JVS(588)*UV(75)+JVS(589)*UV(76)+JVS(590)*UV(78)+JVS(591)*UV(79)+JVS(593)*UV(82)+JVS(594)&
              &*UV(83)+JVS(595)*UV(84)+JVS(596)*UV(85)
  JUV(75) = JVS(597)*UV(23)+JVS(598)*UV(58)+JVS(599)*UV(62)+JVS(600)*UV(64)+JVS(602)*UV(66)+JVS(603)*UV(70)+JVS(604)&
              &*UV(74)+JVS(605)*UV(75)+JVS(606)*UV(76)+JVS(607)*UV(77)+JVS(608)*UV(78)+JVS(609)*UV(79)+JVS(610)*UV(80)&
              &+JVS(611)*UV(81)+JVS(612)*UV(82)+JVS(613)*UV(83)+JVS(614)*UV(84)+JVS(615)*UV(85)+JVS(616)*UV(86)
  JUV(76) = JVS(617)*UV(20)+JVS(618)*UV(27)+JVS(620)*UV(39)+JVS(621)*UV(41)+JVS(622)*UV(42)+JVS(623)*UV(50)+JVS(624)&
              &*UV(54)+JVS(625)*UV(59)+JVS(626)*UV(61)+JVS(627)*UV(62)+JVS(629)*UV(64)+JVS(630)*UV(65)+JVS(631)*UV(66)&
              &+JVS(632)*UV(67)+JVS(636)*UV(72)+JVS(637)*UV(73)+JVS(638)*UV(74)+JVS(639)*UV(75)+JVS(640)*UV(76)+JVS(641)&
              &*UV(77)+JVS(642)*UV(78)+JVS(643)*UV(79)+JVS(644)*UV(80)+JVS(645)*UV(81)+JVS(646)*UV(82)+JVS(647)*UV(83)&
              &+JVS(648)*UV(84)+JVS(649)*UV(85)+JVS(650)*UV(86)
  JUV(77) = JVS(651)*UV(19)+JVS(652)*UV(25)+JVS(653)*UV(26)+JVS(654)*UV(30)+JVS(655)*UV(32)+JVS(656)*UV(34)+JVS(657)&
              &*UV(39)+JVS(658)*UV(40)+JVS(659)*UV(41)+JVS(660)*UV(42)+JVS(661)*UV(43)+JVS(662)*UV(44)+JVS(663)*UV(45)&
              &+JVS(664)*UV(48)+JVS(665)*UV(49)+JVS(666)*UV(53)+JVS(667)*UV(55)+JVS(669)*UV(58)+JVS(670)*UV(59)+JVS(671)&
              &*UV(61)+JVS(672)*UV(62)+JVS(673)*UV(63)+JVS(674)*UV(64)+JVS(675)*UV(65)+JVS(676)*UV(66)+JVS(677)*UV(69)&
              &+JVS(678)*UV(70)+JVS(679)*UV(71)+JVS(680)*UV(72)+JVS(681)*UV(73)+JVS(682)*UV(74)+JVS(683)*UV(75)+JVS(684)&
              &*UV(76)+JVS(685)*UV(77)+JVS(686)*UV(78)+JVS(688)*UV(80)+JVS(689)*UV(81)+JVS(690)*UV(82)+JVS(691)*UV(83)&
              &+JVS(692)*UV(84)+JVS(693)*UV(85)+JVS(694)*UV(86)
  JUV(78) = JVS(695)*UV(29)+JVS(696)*UV(36)+JVS(697)*UV(60)+JVS(704)*UV(70)+JVS(707)*UV(74)+JVS(708)*UV(75)+JVS(709)&
              &*UV(76)+JVS(710)*UV(77)+JVS(711)*UV(78)+JVS(712)*UV(79)+JVS(713)*UV(80)+JVS(714)*UV(81)+JVS(715)*UV(82)&
              &+JVS(716)*UV(83)+JVS(717)*UV(84)+JVS(718)*UV(85)+JVS(719)*UV(86)
  JUV(79) = JVS(720)*UV(20)+JVS(721)*UV(21)+JVS(722)*UV(22)+JVS(723)*UV(23)+JVS(724)*UV(28)+JVS(725)*UV(29)+JVS(726)&
              &*UV(31)+JVS(727)*UV(36)+JVS(728)*UV(37)+JVS(729)*UV(38)+JVS(731)*UV(52)+JVS(733)*UV(57)+JVS(734)*UV(58)&
              &+JVS(735)*UV(59)+JVS(736)*UV(60)+JVS(737)*UV(61)+JVS(741)*UV(65)+JVS(745)*UV(69)+JVS(746)*UV(70)+JVS(750)&
              &*UV(74)+JVS(751)*UV(75)+JVS(752)*UV(76)+JVS(753)*UV(77)+JVS(754)*UV(78)+JVS(755)*UV(79)+JVS(756)*UV(80)&
              &+JVS(757)*UV(81)+JVS(758)*UV(82)+JVS(759)*UV(83)+JVS(760)*UV(84)+JVS(761)*UV(85)+JVS(762)*UV(86)
  JUV(80) = JVS(763)*UV(28)+JVS(764)*UV(38)+JVS(765)*UV(43)+JVS(766)*UV(46)+JVS(767)*UV(47)+JVS(768)*UV(49)+JVS(769)&
              &*UV(50)+JVS(770)*UV(52)+JVS(771)*UV(53)+JVS(772)*UV(55)+JVS(773)*UV(56)+JVS(775)*UV(58)+JVS(776)*UV(59)&
              &+JVS(777)*UV(60)+JVS(778)*UV(61)+JVS(779)*UV(62)+JVS(780)*UV(63)+JVS(781)*UV(64)+JVS(782)*UV(65)+JVS(784)&
              &*UV(67)+JVS(785)*UV(68)+JVS(787)*UV(70)+JVS(788)*UV(71)+JVS(791)*UV(74)+JVS(792)*UV(75)+JVS(793)*UV(76)&
              &+JVS(794)*UV(77)+JVS(795)*UV(78)+JVS(796)*UV(79)+JVS(797)*UV(80)+JVS(798)*UV(81)+JVS(799)*UV(82)+JVS(800)&
              &*UV(83)+JVS(801)*UV(84)+JVS(802)*UV(85)+JVS(803)*UV(86)
  JUV(81) = JVS(804)*UV(15)+JVS(805)*UV(31)+JVS(806)*UV(35)+JVS(807)*UV(39)+JVS(808)*UV(53)+JVS(809)*UV(54)+JVS(810)&
              &*UV(55)+JVS(811)*UV(58)+JVS(814)*UV(63)+JVS(815)*UV(65)+JVS(816)*UV(66)+JVS(817)*UV(67)+JVS(819)*UV(70)&
              &+JVS(823)*UV(74)+JVS(824)*UV(75)+JVS(825)*UV(76)+JVS(826)*UV(77)+JVS(827)*UV(78)+JVS(829)*UV(80)+JVS(830)&
              &*UV(81)+JVS(831)*UV(82)+JVS(832)*UV(83)+JVS(833)*UV(84)+JVS(834)*UV(85)+JVS(835)*UV(86)
  JUV(82) = JVS(836)*UV(14)+JVS(837)*UV(15)+JVS(838)*UV(16)+JVS(839)*UV(19)+JVS(840)*UV(24)+JVS(841)*UV(25)+JVS(842)&
              &*UV(26)+JVS(843)*UV(29)+JVS(844)*UV(30)+JVS(845)*UV(32)+JVS(846)*UV(33)+JVS(847)*UV(34)+JVS(848)*UV(35)&
              &+JVS(849)*UV(38)+JVS(850)*UV(39)+JVS(851)*UV(40)+JVS(852)*UV(41)+JVS(853)*UV(42)+JVS(854)*UV(43)+JVS(855)&
              &*UV(44)+JVS(856)*UV(45)+JVS(857)*UV(47)+JVS(858)*UV(48)+JVS(859)*UV(49)+JVS(860)*UV(50)+JVS(861)*UV(51)&
              &+JVS(862)*UV(52)+JVS(863)*UV(53)+JVS(864)*UV(54)+JVS(865)*UV(55)+JVS(866)*UV(56)+JVS(868)*UV(58)+JVS(869)&
              &*UV(59)+JVS(870)*UV(61)+JVS(871)*UV(62)+JVS(872)*UV(63)+JVS(873)*UV(64)+JVS(874)*UV(65)+JVS(875)*UV(66)&
              &+JVS(876)*UV(67)+JVS(877)*UV(68)+JVS(878)*UV(69)+JVS(880)*UV(71)+JVS(881)*UV(72)+JVS(882)*UV(73)+JVS(883)&
              &*UV(74)+JVS(887)*UV(78)+JVS(888)*UV(79)+JVS(889)*UV(80)+JVS(891)*UV(82)+JVS(894)*UV(85)
  JUV(83) = JVS(896)*UV(21)+JVS(897)*UV(56)+JVS(899)*UV(59)+JVS(900)*UV(61)+JVS(901)*UV(62)+JVS(902)*UV(64)+JVS(903)&
              &*UV(65)+JVS(904)*UV(66)+JVS(906)*UV(71)+JVS(907)*UV(72)+JVS(908)*UV(73)+JVS(909)*UV(74)+JVS(910)*UV(75)&
              &+JVS(911)*UV(76)+JVS(912)*UV(77)+JVS(913)*UV(78)+JVS(914)*UV(79)+JVS(915)*UV(80)+JVS(916)*UV(81)+JVS(917)&
              &*UV(82)+JVS(918)*UV(83)+JVS(919)*UV(84)+JVS(920)*UV(85)+JVS(921)*UV(86)
  JUV(84) = JVS(922)*UV(22)+JVS(923)*UV(47)+JVS(927)*UV(75)+JVS(928)*UV(76)+JVS(929)*UV(77)+JVS(930)*UV(78)+JVS(931)&
              &*UV(79)+JVS(932)*UV(80)+JVS(933)*UV(81)+JVS(934)*UV(82)+JVS(935)*UV(83)+JVS(936)*UV(84)+JVS(937)*UV(85)&
              &+JVS(938)*UV(86)
  JUV(85) = JVS(939)*UV(16)+JVS(940)*UV(24)+JVS(941)*UV(25)+JVS(942)*UV(29)+JVS(943)*UV(33)+JVS(944)*UV(34)+JVS(945)&
              &*UV(35)+JVS(946)*UV(36)+JVS(947)*UV(37)+JVS(948)*UV(38)+JVS(949)*UV(40)+JVS(950)*UV(41)+JVS(951)*UV(42)&
              &+JVS(952)*UV(44)+JVS(953)*UV(45)+JVS(955)*UV(48)+JVS(956)*UV(50)+JVS(957)*UV(51)+JVS(958)*UV(53)+JVS(959)&
              &*UV(55)+JVS(960)*UV(56)+JVS(961)*UV(57)+JVS(962)*UV(58)+JVS(963)*UV(59)+JVS(964)*UV(61)+JVS(965)*UV(62)&
              &+JVS(966)*UV(63)+JVS(967)*UV(64)+JVS(968)*UV(65)+JVS(969)*UV(66)+JVS(970)*UV(67)+JVS(971)*UV(68)+JVS(972)&
              &*UV(69)+JVS(973)*UV(70)+JVS(974)*UV(71)+JVS(976)*UV(73)+JVS(977)*UV(74)+JVS(978)*UV(75)+JVS(979)*UV(76)&
              &+JVS(980)*UV(77)+JVS(981)*UV(78)+JVS(982)*UV(79)+JVS(983)*UV(80)+JVS(984)*UV(81)+JVS(985)*UV(82)+JVS(986)&
              &*UV(83)+JVS(987)*UV(84)+JVS(988)*UV(85)+JVS(989)*UV(86)
  JUV(86) = JVS(990)*UV(26)+JVS(991)*UV(30)+JVS(992)*UV(32)+JVS(993)*UV(34)+JVS(994)*UV(39)+JVS(995)*UV(40)+JVS(996)&
              &*UV(55)+JVS(997)*UV(58)+JVS(998)*UV(59)+JVS(999)*UV(61)+JVS(1000)*UV(63)+JVS(1001)*UV(64)+JVS(1002)*UV(65)&
              &+JVS(1003)*UV(66)+JVS(1004)*UV(69)+JVS(1005)*UV(70)+JVS(1006)*UV(71)+JVS(1007)*UV(72)+JVS(1008)*UV(73)&
              &+JVS(1009)*UV(74)+JVS(1010)*UV(75)+JVS(1011)*UV(76)+JVS(1012)*UV(77)+JVS(1013)*UV(78)+JVS(1015)*UV(80)&
              &+JVS(1016)*UV(81)+JVS(1017)*UV(82)+JVS(1018)*UV(83)+JVS(1019)*UV(84)+JVS(1020)*UV(85)+JVS(1021)*UV(86)
      
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
  JTUV(3) = JVS(20)*UV(3)
  JTUV(4) = JVS(30)*UV(4)
  JTUV(5) = JVS(45)*UV(5)
  JTUV(6) = JVS(48)*UV(6)
  JTUV(7) = JVS(49)*UV(6)+JVS(53)*UV(7)
  JTUV(8) = JVS(46)*UV(5)+JVS(50)*UV(6)+JVS(56)*UV(8)
  JTUV(9) = JVS(61)*UV(9)
  JTUV(10) = JVS(65)*UV(10)
  JTUV(11) = JVS(68)*UV(11)
  JTUV(12) = JVS(73)*UV(12)
  JTUV(13) = JVS(78)*UV(13)
  JTUV(14) = JVS(87)*UV(14)+JVS(483)*UV(70)+JVS(836)*UV(82)
  JTUV(15) = JVS(89)*UV(15)+JVS(804)*UV(81)+JVS(837)*UV(82)
  JTUV(16) = JVS(2)*UV(1)+JVS(91)*UV(16)+JVS(838)*UV(82)+JVS(939)*UV(85)
  JTUV(17) = JVS(54)*UV(7)+JVS(57)*UV(8)+JVS(93)*UV(17)+JVS(97)*UV(18)
  JTUV(18) = JVS(51)*UV(6)+JVS(94)*UV(17)+JVS(98)*UV(18)
  JTUV(19) = JVS(103)*UV(19)+JVS(406)*UV(67)+JVS(651)*UV(77)+JVS(839)*UV(82)
  JTUV(20) = JVS(105)*UV(20)+JVS(617)*UV(76)+JVS(720)*UV(79)
  JTUV(21) = JVS(108)*UV(21)+JVS(721)*UV(79)+JVS(896)*UV(83)
  JTUV(22) = JVS(111)*UV(22)+JVS(722)*UV(79)+JVS(922)*UV(84)
  JTUV(23) = JVS(114)*UV(23)+JVS(597)*UV(75)+JVS(723)*UV(79)
  JTUV(24) = JVS(117)*UV(24)+JVS(840)*UV(82)+JVS(940)*UV(85)
  JTUV(25) = JVS(120)*UV(25)+JVS(407)*UV(67)+JVS(430)*UV(68)+JVS(652)*UV(77)+JVS(841)*UV(82)+JVS(941)*UV(85)
  JTUV(26) = JVS(122)*UV(26)+JVS(280)*UV(54)+JVS(499)*UV(71)+JVS(653)*UV(77)+JVS(842)*UV(82)+JVS(990)*UV(86)
  JTUV(27) = JVS(124)*UV(27)+JVS(618)*UV(76)
  JTUV(28) = JVS(130)*UV(28)+JVS(254)*UV(52)+JVS(724)*UV(79)+JVS(763)*UV(80)
  JTUV(29) = JVS(133)*UV(29)+JVS(695)*UV(78)+JVS(725)*UV(79)+JVS(843)*UV(82)+JVS(942)*UV(85)
  JTUV(30) = JVS(136)*UV(30)+JVS(138)*UV(31)+JVS(281)*UV(54)+JVS(340)*UV(60)+JVS(408)*UV(67)+JVS(431)*UV(68)+JVS(500)&
               &*UV(71)+JVS(529)*UV(72)+JVS(654)*UV(77)+JVS(844)*UV(82)+JVS(991)*UV(86)
  JTUV(31) = JVS(139)*UV(31)+JVS(282)*UV(54)+JVS(469)*UV(69)+JVS(726)*UV(79)+JVS(805)*UV(81)
  JTUV(32) = JVS(142)*UV(32)+JVS(283)*UV(54)+JVS(341)*UV(60)+JVS(409)*UV(67)+JVS(432)*UV(68)+JVS(501)*UV(71)+JVS(530)&
               &*UV(72)+JVS(550)*UV(73)+JVS(655)*UV(77)+JVS(845)*UV(82)+JVS(992)*UV(86)
  JTUV(33) = JVS(144)*UV(33)+JVS(433)*UV(68)+JVS(846)*UV(82)+JVS(943)*UV(85)
  JTUV(34) = JVS(125)*UV(27)+JVS(149)*UV(34)+JVS(172)*UV(41)+JVS(176)*UV(42)+JVS(180)*UV(43)+JVS(188)*UV(45)+JVS(199)&
               &*UV(47)+JVS(217)*UV(50)+JVS(300)*UV(56)+JVS(656)*UV(77)+JVS(847)*UV(82)+JVS(944)*UV(85)+JVS(993)*UV(86)
  JTUV(35) = JVS(151)*UV(35)+JVS(434)*UV(68)+JVS(806)*UV(81)+JVS(848)*UV(82)+JVS(945)*UV(85)
  JTUV(36) = JVS(5)*UV(2)+JVS(155)*UV(36)+JVS(435)*UV(68)+JVS(696)*UV(78)+JVS(727)*UV(79)+JVS(946)*UV(85)
  JTUV(37) = JVS(74)*UV(12)+JVS(79)*UV(13)+JVS(159)*UV(37)+JVS(193)*UV(46)+JVS(728)*UV(79)+JVS(947)*UV(85)
  JTUV(38) = JVS(164)*UV(38)+JVS(729)*UV(79)+JVS(764)*UV(80)+JVS(849)*UV(82)+JVS(948)*UV(85)
  JTUV(39) = JVS(168)*UV(39)+JVS(229)*UV(51)+JVS(284)*UV(54)+JVS(342)*UV(60)+JVS(410)*UV(67)+JVS(436)*UV(68)+JVS(502)&
               &*UV(71)+JVS(531)*UV(72)+JVS(551)*UV(73)+JVS(620)*UV(76)+JVS(657)*UV(77)+JVS(807)*UV(81)+JVS(850)*UV(82)&
               &+JVS(994)*UV(86)
  JTUV(40) = JVS(170)*UV(40)+JVS(173)*UV(41)+JVS(177)*UV(42)+JVS(181)*UV(43)+JVS(189)*UV(45)+JVS(200)*UV(47)+JVS(211)&
               &*UV(49)+JVS(218)*UV(50)+JVS(301)*UV(56)+JVS(552)*UV(73)+JVS(658)*UV(77)+JVS(851)*UV(82)+JVS(949)*UV(85)&
               &+JVS(995)*UV(86)
  JTUV(41) = JVS(174)*UV(41)+JVS(219)*UV(50)+JVS(230)*UV(51)+JVS(302)*UV(56)+JVS(343)*UV(60)+JVS(503)*UV(71)+JVS(621)&
               &*UV(76)+JVS(659)*UV(77)+JVS(852)*UV(82)+JVS(950)*UV(85)
  JTUV(42) = JVS(178)*UV(42)+JVS(220)*UV(50)+JVS(231)*UV(51)+JVS(303)*UV(56)+JVS(344)*UV(60)+JVS(504)*UV(71)+JVS(622)&
               &*UV(76)+JVS(660)*UV(77)+JVS(853)*UV(82)+JVS(951)*UV(85)
  JTUV(43) = JVS(182)*UV(43)+JVS(221)*UV(50)+JVS(255)*UV(52)+JVS(318)*UV(57)+JVS(661)*UV(77)+JVS(765)*UV(80)+JVS(854)&
               &*UV(82)
  JTUV(44) = JVS(6)*UV(2)+JVS(62)*UV(9)+JVS(185)*UV(44)+JVS(232)*UV(51)+JVS(304)*UV(56)+JVS(437)*UV(68)+JVS(574)*UV(74)&
               &+JVS(662)*UV(77)+JVS(855)*UV(82)+JVS(952)*UV(85)
  JTUV(45) = JVS(190)*UV(45)+JVS(233)*UV(51)+JVS(305)*UV(56)+JVS(505)*UV(71)+JVS(575)*UV(74)+JVS(663)*UV(77)+JVS(856)&
               &*UV(82)+JVS(953)*UV(85)
  JTUV(46) = JVS(160)*UV(37)+JVS(194)*UV(46)+JVS(256)*UV(52)+JVS(766)*UV(80)
  JTUV(47) = JVS(80)*UV(13)+JVS(201)*UV(47)+JVS(257)*UV(52)+JVS(767)*UV(80)+JVS(857)*UV(82)+JVS(923)*UV(84)
  JTUV(48) = JVS(206)*UV(48)+JVS(506)*UV(71)+JVS(664)*UV(77)+JVS(858)*UV(82)+JVS(955)*UV(85)
  JTUV(49) = JVS(212)*UV(49)+JVS(258)*UV(52)+JVS(306)*UV(56)+JVS(319)*UV(57)+JVS(665)*UV(77)+JVS(768)*UV(80)+JVS(859)&
               &*UV(82)
  JTUV(50) = JVS(222)*UV(50)+JVS(234)*UV(51)+JVS(259)*UV(52)+JVS(623)*UV(76)+JVS(769)*UV(80)+JVS(860)*UV(82)+JVS(956)&
               &*UV(85)
  JTUV(51) = JVS(235)*UV(51)+JVS(861)*UV(82)+JVS(957)*UV(85)
  JTUV(52) = JVS(260)*UV(52)+JVS(731)*UV(79)+JVS(770)*UV(80)+JVS(862)*UV(82)
  JTUV(53) = JVS(7)*UV(2)+JVS(236)*UV(51)+JVS(275)*UV(53)+JVS(307)*UV(56)+JVS(411)*UV(67)+JVS(438)*UV(68)+JVS(484)&
               &*UV(70)+JVS(507)*UV(71)+JVS(576)*UV(74)+JVS(666)*UV(77)+JVS(771)*UV(80)+JVS(808)*UV(81)+JVS(863)*UV(82)&
               &+JVS(958)*UV(85)
  JTUV(54) = JVS(285)*UV(54)+JVS(345)*UV(60)+JVS(439)*UV(68)+JVS(624)*UV(76)+JVS(809)*UV(81)+JVS(864)*UV(82)
  JTUV(55) = JVS(8)*UV(2)+JVS(21)*UV(3)+JVS(63)*UV(9)+JVS(75)*UV(12)+JVS(81)*UV(13)+JVS(237)*UV(51)+JVS(295)*UV(55)&
               &+JVS(412)*UV(67)+JVS(440)*UV(68)+JVS(485)*UV(70)+JVS(508)*UV(71)+JVS(532)*UV(72)+JVS(577)*UV(74)+JVS(667)&
               &*UV(77)+JVS(772)*UV(80)+JVS(810)*UV(81)+JVS(865)*UV(82)+JVS(959)*UV(85)+JVS(996)*UV(86)
  JTUV(56) = JVS(238)*UV(51)+JVS(261)*UV(52)+JVS(308)*UV(56)+JVS(441)*UV(68)+JVS(773)*UV(80)+JVS(866)*UV(82)+JVS(897)&
               &*UV(83)+JVS(960)*UV(85)
  JTUV(57) = JVS(195)*UV(46)+JVS(213)*UV(49)+JVS(320)*UV(57)+JVS(733)*UV(79)+JVS(961)*UV(85)
  JTUV(58) = JVS(9)*UV(2)+JVS(31)*UV(4)+JVS(99)*UV(18)+JVS(239)*UV(51)+JVS(330)*UV(58)+JVS(346)*UV(60)+JVS(375)*UV(62)&
               &+JVS(387)*UV(64)+JVS(399)*UV(66)+JVS(443)*UV(68)+JVS(486)*UV(70)+JVS(553)*UV(73)+JVS(578)*UV(74)+JVS(598)&
               &*UV(75)+JVS(669)*UV(77)+JVS(734)*UV(79)+JVS(775)*UV(80)+JVS(811)*UV(81)+JVS(868)*UV(82)+JVS(962)*UV(85)&
               &+JVS(997)*UV(86)
  JTUV(59) = JVS(10)*UV(2)+JVS(32)*UV(4)+JVS(126)*UV(27)+JVS(240)*UV(51)+JVS(286)*UV(54)+JVS(310)*UV(56)+JVS(335)*UV(59)&
               &+JVS(347)*UV(60)+JVS(444)*UV(68)+JVS(470)*UV(69)+JVS(487)*UV(70)+JVS(509)*UV(71)+JVS(554)*UV(73)+JVS(579)&
               &*UV(74)+JVS(625)*UV(76)+JVS(670)*UV(77)+JVS(735)*UV(79)+JVS(776)*UV(80)+JVS(869)*UV(82)+JVS(899)*UV(83)&
               &+JVS(963)*UV(85)+JVS(998)*UV(86)
  JTUV(60) = JVS(348)*UV(60)+JVS(697)*UV(78)+JVS(736)*UV(79)+JVS(777)*UV(80)
  JTUV(61) = JVS(11)*UV(2)+JVS(33)*UV(4)+JVS(127)*UV(27)+JVS(241)*UV(51)+JVS(287)*UV(54)+JVS(311)*UV(56)+JVS(349)*UV(60)&
               &+JVS(370)*UV(61)+JVS(445)*UV(68)+JVS(471)*UV(69)+JVS(488)*UV(70)+JVS(510)*UV(71)+JVS(555)*UV(73)+JVS(580)&
               &*UV(74)+JVS(626)*UV(76)+JVS(671)*UV(77)+JVS(737)*UV(79)+JVS(778)*UV(80)+JVS(870)*UV(82)+JVS(900)*UV(83)&
               &+JVS(964)*UV(85)+JVS(999)*UV(86)
  JTUV(62) = JVS(12)*UV(2)+JVS(223)*UV(50)+JVS(242)*UV(51)+JVS(263)*UV(52)+JVS(376)*UV(62)+JVS(446)*UV(68)+JVS(489)&
               &*UV(70)+JVS(511)*UV(71)+JVS(533)*UV(72)+JVS(581)*UV(74)+JVS(599)*UV(75)+JVS(627)*UV(76)+JVS(672)*UV(77)&
               &+JVS(779)*UV(80)+JVS(871)*UV(82)+JVS(901)*UV(83)+JVS(965)*UV(85)
  JTUV(63) = JVS(13)*UV(2)+JVS(22)*UV(3)+JVS(34)*UV(4)+JVS(243)*UV(51)+JVS(288)*UV(54)+JVS(350)*UV(60)+JVS(382)*UV(63)&
               &+JVS(413)*UV(67)+JVS(447)*UV(68)+JVS(472)*UV(69)+JVS(490)*UV(70)+JVS(512)*UV(71)+JVS(534)*UV(72)+JVS(556)&
               &*UV(73)+JVS(582)*UV(74)+JVS(673)*UV(77)+JVS(780)*UV(80)+JVS(814)*UV(81)+JVS(872)*UV(82)+JVS(966)*UV(85)&
               &+JVS(1000)*UV(86)
  JTUV(64) = JVS(14)*UV(2)+JVS(35)*UV(4)+JVS(224)*UV(50)+JVS(244)*UV(51)+JVS(264)*UV(52)+JVS(312)*UV(56)+JVS(388)*UV(64)&
               &+JVS(414)*UV(67)+JVS(448)*UV(68)+JVS(473)*UV(69)+JVS(513)*UV(71)+JVS(535)*UV(72)+JVS(557)*UV(73)+JVS(583)&
               &*UV(74)+JVS(600)*UV(75)+JVS(629)*UV(76)+JVS(674)*UV(77)+JVS(781)*UV(80)+JVS(873)*UV(82)+JVS(902)*UV(83)&
               &+JVS(967)*UV(85)+JVS(1001)*UV(86)
  JTUV(65) = JVS(15)*UV(2)+JVS(23)*UV(3)+JVS(36)*UV(4)+JVS(202)*UV(47)+JVS(245)*UV(51)+JVS(289)*UV(54)+JVS(351)*UV(60)&
               &+JVS(377)*UV(62)+JVS(389)*UV(64)+JVS(394)*UV(65)+JVS(400)*UV(66)+JVS(415)*UV(67)+JVS(449)*UV(68)+JVS(474)&
               &*UV(69)+JVS(491)*UV(70)+JVS(514)*UV(71)+JVS(536)*UV(72)+JVS(558)*UV(73)+JVS(584)*UV(74)+JVS(630)*UV(76)&
               &+JVS(675)*UV(77)+JVS(741)*UV(79)+JVS(782)*UV(80)+JVS(815)*UV(81)+JVS(874)*UV(82)+JVS(903)*UV(83)+JVS(968)&
               &*UV(85)+JVS(1002)*UV(86)
  JTUV(66) = JVS(16)*UV(2)+JVS(225)*UV(50)+JVS(246)*UV(51)+JVS(352)*UV(60)+JVS(401)*UV(66)+JVS(450)*UV(68)+JVS(492)&
               &*UV(70)+JVS(515)*UV(71)+JVS(537)*UV(72)+JVS(559)*UV(73)+JVS(585)*UV(74)+JVS(602)*UV(75)+JVS(631)*UV(76)&
               &+JVS(676)*UV(77)+JVS(816)*UV(81)+JVS(875)*UV(82)+JVS(904)*UV(83)+JVS(969)*UV(85)+JVS(1003)*UV(86)
  JTUV(67) = JVS(247)*UV(51)+JVS(267)*UV(52)+JVS(416)*UV(67)+JVS(632)*UV(76)+JVS(784)*UV(80)+JVS(817)*UV(81)+JVS(876)&
               &*UV(82)+JVS(970)*UV(85)
  JTUV(68) = JVS(156)*UV(36)+JVS(248)*UV(51)+JVS(268)*UV(52)+JVS(451)*UV(68)+JVS(785)*UV(80)+JVS(877)*UV(82)+JVS(971)&
               &*UV(85)
  JTUV(69) = JVS(290)*UV(54)+JVS(353)*UV(60)+JVS(417)*UV(67)+JVS(452)*UV(68)+JVS(475)*UV(69)+JVS(516)*UV(71)+JVS(538)&
               &*UV(72)+JVS(560)*UV(73)+JVS(677)*UV(77)+JVS(745)*UV(79)+JVS(878)*UV(82)+JVS(972)*UV(85)+JVS(1004)*UV(86)
  JTUV(70) = JVS(82)*UV(13)+JVS(249)*UV(51)+JVS(276)*UV(53)+JVS(296)*UV(55)+JVS(313)*UV(56)+JVS(331)*UV(58)+JVS(336)&
               &*UV(59)+JVS(354)*UV(60)+JVS(371)*UV(61)+JVS(378)*UV(62)+JVS(383)*UV(63)+JVS(395)*UV(65)+JVS(402)*UV(66)&
               &+JVS(418)*UV(67)+JVS(453)*UV(68)+JVS(493)*UV(70)+JVS(517)*UV(71)+JVS(539)*UV(72)+JVS(561)*UV(73)+JVS(586)&
               &*UV(74)+JVS(603)*UV(75)+JVS(678)*UV(77)+JVS(704)*UV(78)+JVS(746)*UV(79)+JVS(787)*UV(80)+JVS(819)*UV(81)&
               &+JVS(973)*UV(85)+JVS(1005)*UV(86)
  JTUV(71) = JVS(250)*UV(51)+JVS(269)*UV(52)+JVS(419)*UV(67)+JVS(518)*UV(71)+JVS(679)*UV(77)+JVS(788)*UV(80)+JVS(880)&
               &*UV(82)+JVS(906)*UV(83)+JVS(974)*UV(85)+JVS(1006)*UV(86)
  JTUV(72) = JVS(355)*UV(60)+JVS(420)*UV(67)+JVS(454)*UV(68)+JVS(519)*UV(71)+JVS(540)*UV(72)+JVS(636)*UV(76)+JVS(680)&
               &*UV(77)+JVS(881)*UV(82)+JVS(907)*UV(83)+JVS(1007)*UV(86)
  JTUV(73) = JVS(356)*UV(60)+JVS(421)*UV(67)+JVS(455)*UV(68)+JVS(520)*UV(71)+JVS(541)*UV(72)+JVS(562)*UV(73)+JVS(637)&
               &*UV(76)+JVS(681)*UV(77)+JVS(882)*UV(82)+JVS(908)*UV(83)+JVS(976)*UV(85)+JVS(1008)*UV(86)
  JTUV(74) = JVS(17)*UV(2)+JVS(24)*UV(3)+JVS(37)*UV(4)+JVS(64)*UV(9)+JVS(83)*UV(13)+JVS(88)*UV(14)+JVS(128)*UV(27)&
               &+JVS(186)*UV(44)+JVS(191)*UV(45)+JVS(203)*UV(47)+JVS(226)*UV(50)+JVS(251)*UV(51)+JVS(277)*UV(53)+JVS(291)&
               &*UV(54)+JVS(297)*UV(55)+JVS(314)*UV(56)+JVS(332)*UV(58)+JVS(337)*UV(59)+JVS(357)*UV(60)+JVS(372)*UV(61)&
               &+JVS(379)*UV(62)+JVS(384)*UV(63)+JVS(391)*UV(64)+JVS(396)*UV(65)+JVS(403)*UV(66)+JVS(422)*UV(67)+JVS(456)&
               &*UV(68)+JVS(494)*UV(70)+JVS(521)*UV(71)+JVS(542)*UV(72)+JVS(563)*UV(73)+JVS(587)*UV(74)+JVS(604)*UV(75)&
               &+JVS(638)*UV(76)+JVS(682)*UV(77)+JVS(707)*UV(78)+JVS(750)*UV(79)+JVS(791)*UV(80)+JVS(823)*UV(81)+JVS(883)&
               &*UV(82)+JVS(909)*UV(83)+JVS(977)*UV(85)+JVS(1009)*UV(86)
  JTUV(75) = JVS(38)*UV(4)+JVS(69)*UV(11)+JVS(115)*UV(23)+JVS(321)*UV(57)+JVS(358)*UV(60)+JVS(423)*UV(67)+JVS(457)&
               &*UV(68)+JVS(588)*UV(74)+JVS(605)*UV(75)+JVS(639)*UV(76)+JVS(683)*UV(77)+JVS(708)*UV(78)+JVS(751)*UV(79)&
               &+JVS(792)*UV(80)+JVS(824)*UV(81)+JVS(910)*UV(83)+JVS(927)*UV(84)+JVS(978)*UV(85)+JVS(1010)*UV(86)
  JTUV(76) = JVS(25)*UV(3)+JVS(66)*UV(10)+JVS(106)*UV(20)+JVS(322)*UV(57)+JVS(359)*UV(60)+JVS(424)*UV(67)+JVS(458)&
               &*UV(68)+JVS(564)*UV(73)+JVS(589)*UV(74)+JVS(606)*UV(75)+JVS(640)*UV(76)+JVS(684)*UV(77)+JVS(709)*UV(78)&
               &+JVS(752)*UV(79)+JVS(793)*UV(80)+JVS(825)*UV(81)+JVS(911)*UV(83)+JVS(928)*UV(84)+JVS(979)*UV(85)+JVS(1011)&
               &*UV(86)
  JTUV(77) = JVS(26)*UV(3)+JVS(39)*UV(4)+JVS(145)*UV(33)+JVS(207)*UV(48)+JVS(360)*UV(60)+JVS(459)*UV(68)+JVS(543)*UV(72)&
               &+JVS(565)*UV(73)+JVS(607)*UV(75)+JVS(641)*UV(76)+JVS(685)*UV(77)+JVS(710)*UV(78)+JVS(753)*UV(79)+JVS(794)&
               &*UV(80)+JVS(826)*UV(81)+JVS(912)*UV(83)+JVS(929)*UV(84)+JVS(980)*UV(85)+JVS(1012)*UV(86)
  JTUV(78) = JVS(18)*UV(2)+JVS(58)*UV(8)+JVS(100)*UV(18)+JVS(134)*UV(29)+JVS(157)*UV(36)+JVS(323)*UV(57)+JVS(361)*UV(60)&
               &+JVS(425)*UV(67)+JVS(460)*UV(68)+JVS(478)*UV(69)+JVS(495)*UV(70)+JVS(590)*UV(74)+JVS(608)*UV(75)+JVS(642)&
               &*UV(76)+JVS(686)*UV(77)+JVS(711)*UV(78)+JVS(754)*UV(79)+JVS(795)*UV(80)+JVS(827)*UV(81)+JVS(887)*UV(82)&
               &+JVS(913)*UV(83)+JVS(930)*UV(84)+JVS(981)*UV(85)+JVS(1013)*UV(86)
  JTUV(79) = JVS(76)*UV(12)+JVS(84)*UV(13)+JVS(107)*UV(20)+JVS(109)*UV(21)+JVS(112)*UV(22)+JVS(116)*UV(23)+JVS(131)&
               &*UV(28)+JVS(140)*UV(31)+JVS(161)*UV(37)+JVS(165)*UV(38)+JVS(196)*UV(46)+JVS(271)*UV(52)+JVS(324)*UV(57)&
               &+JVS(479)*UV(69)+JVS(496)*UV(70)+JVS(591)*UV(74)+JVS(609)*UV(75)+JVS(643)*UV(76)+JVS(712)*UV(78)+JVS(755)&
               &*UV(79)+JVS(796)*UV(80)+JVS(888)*UV(82)+JVS(914)*UV(83)+JVS(931)*UV(84)+JVS(982)*UV(85)
  JTUV(80) = JVS(77)*UV(12)+JVS(85)*UV(13)+JVS(132)*UV(28)+JVS(162)*UV(37)+JVS(183)*UV(43)+JVS(197)*UV(46)+JVS(204)&
               &*UV(47)+JVS(214)*UV(49)+JVS(227)*UV(50)+JVS(252)*UV(51)+JVS(272)*UV(52)+JVS(278)*UV(53)+JVS(293)*UV(54)&
               &+JVS(298)*UV(55)+JVS(315)*UV(56)+JVS(325)*UV(57)+JVS(333)*UV(58)+JVS(338)*UV(59)+JVS(363)*UV(60)+JVS(373)&
               &*UV(61)+JVS(380)*UV(62)+JVS(385)*UV(63)+JVS(392)*UV(64)+JVS(397)*UV(65)+JVS(404)*UV(66)+JVS(426)*UV(67)&
               &+JVS(462)*UV(68)+JVS(480)*UV(69)+JVS(497)*UV(70)+JVS(525)*UV(71)+JVS(546)*UV(72)+JVS(610)*UV(75)+JVS(644)&
               &*UV(76)+JVS(688)*UV(77)+JVS(713)*UV(78)+JVS(756)*UV(79)+JVS(797)*UV(80)+JVS(829)*UV(81)+JVS(889)*UV(82)&
               &+JVS(915)*UV(83)+JVS(932)*UV(84)+JVS(983)*UV(85)+JVS(1015)*UV(86)
  JTUV(81) = JVS(27)*UV(3)+JVS(40)*UV(4)+JVS(146)*UV(33)+JVS(152)*UV(35)+JVS(364)*UV(60)+JVS(463)*UV(68)+JVS(547)*UV(72)&
               &+JVS(569)*UV(73)+JVS(611)*UV(75)+JVS(645)*UV(76)+JVS(689)*UV(77)+JVS(714)*UV(78)+JVS(757)*UV(79)+JVS(798)&
               &*UV(80)+JVS(830)*UV(81)+JVS(916)*UV(83)+JVS(933)*UV(84)+JVS(984)*UV(85)+JVS(1016)*UV(86)
  JTUV(82) = JVS(3)*UV(1)+JVS(19)*UV(2)+JVS(52)*UV(6)+JVS(55)*UV(7)+JVS(59)*UV(8)+JVS(86)*UV(13)+JVS(90)*UV(15)+JVS(92)&
               &*UV(16)+JVS(95)*UV(17)+JVS(101)*UV(18)+JVS(104)*UV(19)+JVS(118)*UV(24)+JVS(121)*UV(25)+JVS(123)*UV(26)&
               &+JVS(129)*UV(27)+JVS(135)*UV(29)+JVS(137)*UV(30)+JVS(141)*UV(31)+JVS(143)*UV(32)+JVS(147)*UV(33)+JVS(150)&
               &*UV(34)+JVS(153)*UV(35)+JVS(166)*UV(38)+JVS(169)*UV(39)+JVS(171)*UV(40)+JVS(175)*UV(41)+JVS(179)*UV(42)&
               &+JVS(184)*UV(43)+JVS(187)*UV(44)+JVS(192)*UV(45)+JVS(205)*UV(47)+JVS(208)*UV(48)+JVS(215)*UV(49)+JVS(228)&
               &*UV(50)+JVS(253)*UV(51)+JVS(273)*UV(52)+JVS(279)*UV(53)+JVS(294)*UV(54)+JVS(299)*UV(55)+JVS(316)*UV(56)&
               &+JVS(326)*UV(57)+JVS(334)*UV(58)+JVS(339)*UV(59)+JVS(365)*UV(60)+JVS(374)*UV(61)+JVS(381)*UV(62)+JVS(386)&
               &*UV(63)+JVS(393)*UV(64)+JVS(398)*UV(65)+JVS(405)*UV(66)+JVS(427)*UV(67)+JVS(464)*UV(68)+JVS(481)*UV(69)&
               &+JVS(526)*UV(71)+JVS(548)*UV(72)+JVS(570)*UV(73)+JVS(593)*UV(74)+JVS(612)*UV(75)+JVS(646)*UV(76)+JVS(690)&
               &*UV(77)+JVS(715)*UV(78)+JVS(758)*UV(79)+JVS(799)*UV(80)+JVS(831)*UV(81)+JVS(891)*UV(82)+JVS(917)*UV(83)&
               &+JVS(934)*UV(84)+JVS(985)*UV(85)+JVS(1017)*UV(86)
  JTUV(83) = JVS(41)*UV(4)+JVS(70)*UV(11)+JVS(110)*UV(21)+JVS(327)*UV(57)+JVS(366)*UV(60)+JVS(428)*UV(67)+JVS(465)&
               &*UV(68)+JVS(571)*UV(73)+JVS(594)*UV(74)+JVS(613)*UV(75)+JVS(647)*UV(76)+JVS(691)*UV(77)+JVS(716)*UV(78)&
               &+JVS(759)*UV(79)+JVS(800)*UV(80)+JVS(832)*UV(81)+JVS(918)*UV(83)+JVS(935)*UV(84)+JVS(986)*UV(85)+JVS(1018)&
               &*UV(86)
  JTUV(84) = JVS(42)*UV(4)+JVS(71)*UV(11)+JVS(113)*UV(22)+JVS(328)*UV(57)+JVS(367)*UV(60)+JVS(429)*UV(67)+JVS(466)&
               &*UV(68)+JVS(572)*UV(73)+JVS(595)*UV(74)+JVS(614)*UV(75)+JVS(648)*UV(76)+JVS(692)*UV(77)+JVS(717)*UV(78)&
               &+JVS(760)*UV(79)+JVS(801)*UV(80)+JVS(833)*UV(81)+JVS(919)*UV(83)+JVS(936)*UV(84)+JVS(987)*UV(85)+JVS(1019)&
               &*UV(86)
  JTUV(85) = JVS(28)*UV(3)+JVS(43)*UV(4)+JVS(47)*UV(5)+JVS(60)*UV(8)+JVS(67)*UV(10)+JVS(72)*UV(11)+JVS(96)*UV(17)&
               &+JVS(102)*UV(18)+JVS(119)*UV(24)+JVS(154)*UV(35)+JVS(158)*UV(36)+JVS(163)*UV(37)+JVS(167)*UV(38)+JVS(198)&
               &*UV(46)+JVS(209)*UV(48)+JVS(216)*UV(49)+JVS(274)*UV(52)+JVS(329)*UV(57)+JVS(368)*UV(60)+JVS(467)*UV(68)&
               &+JVS(596)*UV(74)+JVS(615)*UV(75)+JVS(649)*UV(76)+JVS(693)*UV(77)+JVS(718)*UV(78)+JVS(761)*UV(79)+JVS(802)&
               &*UV(80)+JVS(834)*UV(81)+JVS(894)*UV(82)+JVS(920)*UV(83)+JVS(937)*UV(84)+JVS(988)*UV(85)+JVS(1020)*UV(86)
  JTUV(86) = JVS(29)*UV(3)+JVS(44)*UV(4)+JVS(148)*UV(33)+JVS(210)*UV(48)+JVS(369)*UV(60)+JVS(468)*UV(68)+JVS(482)*UV(69)&
               &+JVS(549)*UV(72)+JVS(573)*UV(73)+JVS(616)*UV(75)+JVS(650)*UV(76)+JVS(694)*UV(77)+JVS(719)*UV(78)+JVS(762)&
               &*UV(79)+JVS(803)*UV(80)+JVS(835)*UV(81)+JVS(921)*UV(83)+JVS(938)*UV(84)+JVS(989)*UV(85)+JVS(1021)*UV(86)
      
END SUBROUTINE JacTR_SP_Vec

! End of JacTR_SP_Vec function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE saprc99_Jacobian

