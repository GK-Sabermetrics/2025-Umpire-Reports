# Umpire Report Execute File

suppressWarnings(suppressMessages({
  library(tidyverse)
  library(kableExtra)
  library(gridExtra)
  library(gtsummary)
  library(pak)
  library(kableExtra)
  library(knitr)
  library(pandoc)
  library(scales)
  library(ggrepel)
  library(ggplot2)
}))


file = read.csv("20241031-MercerUniversity-Private-1_unverified (1) copy.csv")

xmin = -0.708
xmax = 0.708
ymin = 1.5
ymax = 3.5

gamefile = file %>% 
  filter(TaggedPitchType != 'Other') %>% 
  mutate(
    Count = paste(Balls, Strikes, sep = "-"), .after = "Outs",
    Pitch = TaggedPitchType,
    Pitch = recode(Pitch, Fastball = "FB", TwoSeamFastBall = "2SFB", Sinker = 'SI', 
                   Cutter = 'CT', Splitter = 'SP', ChangeUp = 'CH', Slider = 'SL',
                   Curveball = 'CB', KnuckleBall = 'KC'),
    PitchCall = recode(PitchCall, BallCalled = 'Ball', BallinDirt = 'Ball',
                       FoulBallNotFieldable = 'Foul', FoulBallFieldable = 'Foul', 
                       StrikeCalled = 'Strike'),
    Top.Bottom = recode(Top.Bottom, Top = "T", Bottom = "B"),
    Inn = paste(Top.Bottom, Inning, sep = " "),
    KorBB = recode(KorBB, Strikeout = 'Strikeout', Walk = 'Walk', Undefined = ""),
    InZone = ifelse(between(PlateLocHeight, 1.3795,3.6205) & between(PlateLocSide, -.828, .828), 1, 0),
    OutZone = ifelse(InZone == 1, 0,1),
    MissedCall = case_when(
      PitchCall == 'Strike' & InZone == 0 ~ 1,
      PitchCall == 'Ball' & InZone == 1 ~ 1,
      .default = 0
      ),
    Likely = ifelse(PitchCall == 'Strike' & MissedCall == 1, "Ball", "Strike"),
    ZoneDistance = mapply(
      point_to_rectangle_distance,
      PlateLocSide, PlateLocHeight,
      MoreArgs = list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
    ),
    ZoneDistance = round(ZoneDistance*12,1)
  ) %>% 
  rename(
    PAOutcome = KorBB,
    PitchType = TaggedPitchType,
    HitType = TaggedHitType,
    Velo = RelSpeed,
    Spin = SpinRate,
    IVB = InducedVertBreak,
    HB = HorzBreak
  )

opponent = "Mercer"

source('UmpReportFunctions.R')

UmpireReport(gamefile = gamefile)

