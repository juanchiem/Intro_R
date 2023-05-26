## Nasapower

Meteo de Balcarce durante 2020

```{r, eval =FALSE}
pacman::p_load(nasapower)

bce_rad_2018 <- get_power(
  community = "AG",
  lonlat =  c(-58.3, -37.75),
  pars = c("ALLSKY_SFC_SW_DWN"),
  dates = c("2018-1-1", "2018-12-30"),
  temporal_api = "daily"
) 
bce_rad_2018

# bce_rad_2018 %>% rio::export("data/bce_rad_2018.csv")
```


```{r}
bce_rad_2018 <- rio::import("data/bce_rad_2018.csv") %>% 
  mutate(date=ymd(YYYYMMDD))
bce_rad_2018
```

Comparamos la estacion meteorologica de la EEA con los datos de nasapower

```{r}
janitor::compare_df_cols(bce_full, bce_rad_2018)

bce_full %>% 
  left_join(bce_rad_2018, 
            by = c("fecha" = "date")) -> bce_full_nasa

bce_full_nasa
```

```{r}
bce_full_nasa %>% 
  ggplot() + 
  aes(x=rad, y=ALLSKY_SFC_SW_DWN) + 
  geom_point() + 
  geom_smooth(method="lm")
```

Clima de Balcarce

```{r}
bce_clima <- get_power(
  community = "ag",
  pars = c("RH2M", "T2M", "ALLSKY_SFC_SW_DWN"),
  lonlat = c(-58.3, -37.75),
  temporal_api = "climatology"
)
bce_clima
```

```{r}
export(bce_full_nasa, file="data/bce_wea_2018.xlsx")
```
