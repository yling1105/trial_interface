import streamlit as st
from multiapp import MultiApp
from apps import (
    basemaps,
    census,
    cesium,
    deck,
    device_loc,
    gee,
    gee_datasets,
    heatmap,
    home,
    housing,
    plotly_maps,
    raster,
    timelapse,
    vector,
    wms,
    xy,
)

st.set_page_config(layout="wide")

apps = MultiApp()

# Add different apps

