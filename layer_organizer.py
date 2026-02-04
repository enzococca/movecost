# -*- coding: utf-8 -*-
"""
Layer Organizer for MOVECOST plugin
Organizes output layers into groups and applies styles
"""

import os
from qgis.core import (
    QgsProject, QgsLayerTreeGroup, QgsRasterLayer, QgsVectorLayer,
    QgsRasterBandStats, QgsSingleBandPseudoColorRenderer,
    QgsColorRampShader, QgsRasterShader, QgsStyle, QgsGradientColorRamp,
    QgsGradientStop, QgsHillshadeRenderer, QgsMultiBandColorRenderer,
    QgsContrastEnhancement, QgsRasterPipe
)
from qgis.PyQt.QtGui import QColor
from qgis.PyQt.QtXml import QDomDocument
from qgis.PyQt.QtCore import QObject, pyqtSignal


class MovecostLayerOrganizer(QObject):
    """Organizes movecost output layers into groups and applies styles."""

    # Signal emitted when organization is complete
    organization_complete = pyqtSignal()

    def __init__(self, parent=None):
        super().__init__(parent)
        self.plugin_dir = os.path.dirname(__file__)
        self.styles_dir = os.path.join(self.plugin_dir, 'styles')
        self._monitoring = False
        self._pending_layers = []

    def start_monitoring(self):
        """Start monitoring for new layers added to the project."""
        if not self._monitoring:
            QgsProject.instance().layersAdded.connect(self._on_layers_added)
            self._monitoring = True
            self._pending_layers = []

    def stop_monitoring(self):
        """Stop monitoring for new layers."""
        if self._monitoring:
            try:
                QgsProject.instance().layersAdded.disconnect(self._on_layers_added)
            except:
                pass
            self._monitoring = False

    def _on_layers_added(self, layers):
        """Handle newly added layers."""
        # Check if these are movecost-related layers
        movecost_keywords = ['Output_', 'LCP', 'Isoline', 'DTM', 'Accumulation',
                            'Corridor', 'Cost', 'Alloc', 'Area', 'netw']

        for layer in layers:
            layer_name = layer.name()
            if any(kw.lower() in layer_name.lower() for kw in movecost_keywords):
                self._pending_layers.append(layer)

    def organize_layers(self, group_name="Movecost Results"):
        """Organize pending layers into groups and apply styles."""
        if not self._pending_layers:
            return

        root = QgsProject.instance().layerTreeRoot()

        # Create main group
        main_group = root.findGroup(group_name)
        if main_group is None:
            main_group = root.insertGroup(0, group_name)

        # Create subgroups (Raster first so it's below Vector)
        raster_group = main_group.findGroup("Raster")
        if raster_group is None:
            raster_group = main_group.insertGroup(0, "Raster")

        vector_group = main_group.findGroup("Vector")
        if vector_group is None:
            vector_group = main_group.insertGroup(0, "Vector")

        # Organize layers
        for layer in self._pending_layers:
            # Find current layer in tree
            layer_node = root.findLayer(layer.id())
            if layer_node is None:
                continue

            # Clone the layer node
            clone = layer_node.clone()

            # Move to appropriate group
            if isinstance(layer, QgsRasterLayer):
                raster_group.addChildNode(clone)
                # Apply style
                self._apply_raster_style(layer)
            elif isinstance(layer, QgsVectorLayer):
                vector_group.addChildNode(clone)
                # Apply style
                self._apply_vector_style(layer)

            # Remove from original location
            parent = layer_node.parent()
            if parent:
                parent.removeChildNode(layer_node)

        # Clear pending layers
        self._pending_layers = []
        self.stop_monitoring()
        self.organization_complete.emit()

    def _apply_raster_style(self, layer):
        """Apply appropriate style to a raster layer."""
        layer_name = layer.name().lower()

        # Determine which style to apply
        if 'dtm' in layer_name:
            self._apply_dtm_style(layer)
        elif any(kw in layer_name for kw in ['cost', 'accumulation', 'corridor', 'alloc']):
            self._apply_cost_style(layer)

    def _apply_vector_style(self, layer):
        """Apply appropriate style to a vector layer."""
        layer_name = layer.name().lower()
        geom_type = layer.geometryType()  # 0=Point, 1=Line, 2=Polygon

        # Determine style based on layer name and geometry type
        if geom_type == 1:  # Line
            if 'lcp' in layer_name and 'back' not in layer_name:
                qml_path = os.path.join(self.styles_dir, 'lcp_style.qml')
            elif 'lcp' in layer_name and 'back' in layer_name:
                qml_path = os.path.join(self.styles_dir, 'lcp_style.qml')
            elif 'isoline' in layer_name:
                qml_path = os.path.join(self.styles_dir, 'isoline_style.qml')
            elif 'netw' in layer_name or 'network' in layer_name:
                qml_path = os.path.join(self.styles_dir, 'network_style.qml')
            else:
                qml_path = os.path.join(self.styles_dir, 'lcp_style.qml')
        elif geom_type == 0:  # Point
            if any(kw in layer_name for kw in ['cost', 'w_cost', 'dest']):
                qml_path = os.path.join(self.styles_dir, 'points_cost_style.qml')
            else:
                return  # No style for generic points
        elif geom_type == 2:  # Polygon
            qml_path = os.path.join(self.styles_dir, 'area_style.qml')
        else:
            return

        if os.path.exists(qml_path):
            layer.loadNamedStyle(qml_path)
            layer.triggerRepaint()

    def _apply_dtm_style(self, layer):
        """Apply topographic color style with hillshade to DTM layer."""
        # Apply colored hillshade effect
        self._apply_colored_hillshade(layer)

    def _apply_cost_style(self, layer):
        """Apply cost surface color style."""
        # Try to load QML file first
        qml_path = os.path.join(self.styles_dir, 'cost_style.qml')
        if os.path.exists(qml_path):
            layer.loadNamedStyle(qml_path)
            layer.triggerRepaint()
            return

        # Fallback: create style programmatically
        self._apply_cost_colors(layer)

    def _apply_colored_hillshade(self, layer):
        """Apply terrain colors with hillshade effect to a raster layer."""
        provider = layer.dataProvider()
        stats = provider.bandStatistics(1, QgsRasterBandStats.All)

        min_val = stats.minimumValue
        max_val = stats.maximumValue

        # Create color ramp shader with terrain colors
        shader = QgsRasterShader()
        color_ramp = QgsColorRampShader()
        color_ramp.setColorRampType(QgsColorRampShader.Interpolated)

        # Terrain colors similar to movecost R output
        color_list = [
            QgsColorRampShader.ColorRampItem(min_val, QColor(0, 97, 0), f'{min_val:.0f}m'),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.05, QColor(56, 168, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.1, QColor(121, 201, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.15, QColor(194, 230, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.2, QColor(255, 255, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.3, QColor(255, 235, 176), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.4, QColor(255, 195, 128), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.5, QColor(228, 156, 93), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.6, QColor(191, 129, 45), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.7, QColor(140, 115, 100), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.8, QColor(161, 145, 141), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.9, QColor(200, 200, 200), ''),
            QgsColorRampShader.ColorRampItem(max_val, QColor(255, 255, 255), f'{max_val:.0f}m'),
        ]

        color_ramp.setColorRampItemList(color_list)
        shader.setRasterShaderFunction(color_ramp)

        # Create pseudocolor renderer
        renderer = QgsSingleBandPseudoColorRenderer(provider, 1, shader)
        layer.setRenderer(renderer)

        # Add hillshade effect via layer's pipe (brightness/contrast simulation)
        # This creates a 3D-like effect
        pipe = layer.pipe()
        if pipe:
            # Set resampling for smoother appearance
            resample_filter = pipe.resampleFilter()
            if resample_filter:
                resample_filter.setZoomedInResampler(None)
                resample_filter.setZoomedOutResampler(None)

        # Also create a hillshade duplicate layer for proper relief effect
        self._add_hillshade_overlay(layer)

        layer.triggerRepaint()

    def _add_hillshade_overlay(self, dtm_layer):
        """Add a hillshade layer on top of the DTM with multiply blend mode."""
        # Clone the DTM layer as hillshade
        source = dtm_layer.source()
        hillshade_layer = QgsRasterLayer(source, dtm_layer.name() + "_hillshade")

        if not hillshade_layer.isValid():
            return

        # Apply hillshade renderer
        renderer = QgsHillshadeRenderer(hillshade_layer.dataProvider(), 1, 315, 45)
        renderer.setZFactor(1.0)
        hillshade_layer.setRenderer(renderer)

        # Set blend mode to multiply for overlay effect
        hillshade_layer.setBlendMode(6)  # 6 = Multiply blend mode
        hillshade_layer.setOpacity(0.5)

        # Add to project
        QgsProject.instance().addMapLayer(hillshade_layer, False)

        # Find and add to the Raster group
        root = QgsProject.instance().layerTreeRoot()
        main_group = root.findGroup("Movecost Results")
        if main_group:
            raster_group = main_group.findGroup("Raster")
            if raster_group:
                # Insert hillshade above the DTM
                raster_group.insertLayer(0, hillshade_layer)

    def _apply_terrain_colors(self, layer):
        """Apply terrain color ramp to a raster layer (legacy method)."""
        provider = layer.dataProvider()
        stats = provider.bandStatistics(1, QgsRasterBandStats.All)

        min_val = stats.minimumValue
        max_val = stats.maximumValue

        # Create color ramp shader
        shader = QgsRasterShader()
        color_ramp = QgsColorRampShader()
        color_ramp.setColorRampType(QgsColorRampShader.Interpolated)

        # Terrain colors
        color_list = [
            QgsColorRampShader.ColorRampItem(min_val, QColor(0, 97, 0), f'{min_val:.0f}m'),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.1, QColor(56, 168, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.2, QColor(121, 201, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.3, QColor(194, 230, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.4, QColor(255, 255, 0), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.5, QColor(255, 235, 176), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.6, QColor(255, 195, 128), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.7, QColor(228, 156, 93), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.8, QColor(191, 129, 45), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.9, QColor(161, 145, 141), ''),
            QgsColorRampShader.ColorRampItem(max_val, QColor(255, 255, 255), f'{max_val:.0f}m'),
        ]

        color_ramp.setColorRampItemList(color_list)
        shader.setRasterShaderFunction(color_ramp)

        renderer = QgsSingleBandPseudoColorRenderer(provider, 1, shader)
        layer.setRenderer(renderer)
        layer.triggerRepaint()

    def _apply_cost_colors(self, layer):
        """Apply cost surface color ramp (green to red)."""
        provider = layer.dataProvider()
        stats = provider.bandStatistics(1, QgsRasterBandStats.All)

        min_val = stats.minimumValue
        max_val = stats.maximumValue

        # Create color ramp shader
        shader = QgsRasterShader()
        color_ramp = QgsColorRampShader()
        color_ramp.setColorRampType(QgsColorRampShader.Interpolated)

        # Cost colors (green to red)
        color_list = [
            QgsColorRampShader.ColorRampItem(min_val, QColor(26, 150, 65), 'Low cost'),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.25, QColor(166, 217, 106), ''),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.5, QColor(255, 255, 192), 'Medium'),
            QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.75, QColor(253, 174, 97), ''),
            QgsColorRampShader.ColorRampItem(max_val, QColor(215, 25, 28), 'High cost'),
        ]

        color_ramp.setColorRampItemList(color_list)
        shader.setRasterShaderFunction(color_ramp)

        renderer = QgsSingleBandPseudoColorRenderer(provider, 1, shader)
        renderer.setOpacity(0.8)
        layer.setRenderer(renderer)
        layer.triggerRepaint()


def organize_movecost_layers(group_name="Movecost Results"):
    """
    Utility function to organize all movecost-related layers in the current project.
    Can be called from Python console or after running algorithms.
    """
    root = QgsProject.instance().layerTreeRoot()

    # Create main group
    main_group = root.findGroup(group_name)
    if main_group is None:
        main_group = root.insertGroup(0, group_name)

    # Create subgroups (Raster first so it's below Vector)
    raster_group = main_group.findGroup("Raster")
    if raster_group is None:
        raster_group = main_group.insertGroup(0, "Raster")

    vector_group = main_group.findGroup("Vector")
    if vector_group is None:
        vector_group = main_group.insertGroup(0, "Vector")

    # Find movecost-related layers
    movecost_keywords = ['Output_', 'LCP', 'Isoline', 'DTM', 'Accumulation',
                        'Corridor', 'Cost', 'Alloc', 'Area', 'netw', 'W_Cost']

    plugin_dir = os.path.dirname(__file__)
    styles_dir = os.path.join(plugin_dir, 'styles')

    # Process all layers in the project
    for layer in QgsProject.instance().mapLayers().values():
        layer_name = layer.name()

        # Check if it's a movecost layer
        if not any(kw.lower() in layer_name.lower() for kw in movecost_keywords):
            continue

        # Find layer node
        layer_node = root.findLayer(layer.id())
        if layer_node is None:
            continue

        # Check if already in our groups
        parent = layer_node.parent()
        if parent and (parent.name() == "Raster" or parent.name() == "Vector"):
            parent_of_parent = parent.parent()
            if parent_of_parent and parent_of_parent.name() == group_name:
                # Already organized, just apply styles
                if isinstance(layer, QgsRasterLayer):
                    _apply_raster_style_to_layer(layer, styles_dir)
                elif isinstance(layer, QgsVectorLayer):
                    _apply_vector_style_to_layer(layer, styles_dir)
                continue

        # Clone and move
        clone = layer_node.clone()

        if isinstance(layer, QgsRasterLayer):
            raster_group.addChildNode(clone)
            _apply_raster_style_to_layer(layer, styles_dir)
        elif isinstance(layer, QgsVectorLayer):
            vector_group.addChildNode(clone)
            _apply_vector_style_to_layer(layer, styles_dir)

        # Remove from original location
        if parent:
            parent.removeChildNode(layer_node)

    return main_group


def _apply_raster_style_to_layer(layer, styles_dir):
    """Apply appropriate style to a raster layer."""
    layer_name = layer.name().lower()

    if 'dtm' in layer_name and 'hillshade' not in layer_name:
        # Apply colored hillshade for DTM
        _apply_colored_hillshade_to_layer(layer)
    elif any(kw in layer_name for kw in ['cost', 'accumulation', 'corridor', 'alloc']):
        qml_path = os.path.join(styles_dir, 'cost_style.qml')
        if os.path.exists(qml_path):
            layer.loadNamedStyle(qml_path)
            layer.triggerRepaint()


def _apply_colored_hillshade_to_layer(layer):
    """Apply terrain colors with hillshade effect to a DTM layer."""
    from qgis.core import QgsRasterBandStats, QgsSingleBandPseudoColorRenderer, QgsColorRampShader, QgsRasterShader, QgsHillshadeRenderer

    provider = layer.dataProvider()
    stats = provider.bandStatistics(1, QgsRasterBandStats.All)

    min_val = stats.minimumValue
    max_val = stats.maximumValue

    # Create color ramp shader with terrain colors
    shader = QgsRasterShader()
    color_ramp = QgsColorRampShader()
    color_ramp.setColorRampType(QgsColorRampShader.Interpolated)

    # Terrain colors similar to movecost R output
    color_list = [
        QgsColorRampShader.ColorRampItem(min_val, QColor(0, 97, 0), f'{min_val:.0f}m'),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.05, QColor(56, 168, 0), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.1, QColor(121, 201, 0), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.15, QColor(194, 230, 0), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.2, QColor(255, 255, 0), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.3, QColor(255, 235, 176), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.4, QColor(255, 195, 128), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.5, QColor(228, 156, 93), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.6, QColor(191, 129, 45), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.7, QColor(140, 115, 100), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.8, QColor(161, 145, 141), ''),
        QgsColorRampShader.ColorRampItem(min_val + (max_val-min_val)*0.9, QColor(200, 200, 200), ''),
        QgsColorRampShader.ColorRampItem(max_val, QColor(255, 255, 255), f'{max_val:.0f}m'),
    ]

    color_ramp.setColorRampItemList(color_list)
    shader.setRasterShaderFunction(color_ramp)

    # Create pseudocolor renderer
    renderer = QgsSingleBandPseudoColorRenderer(provider, 1, shader)
    layer.setRenderer(renderer)

    # Add hillshade overlay layer
    source = layer.source()
    hillshade_layer = QgsRasterLayer(source, layer.name() + "_hillshade")

    if hillshade_layer.isValid():
        # Apply hillshade renderer
        hs_renderer = QgsHillshadeRenderer(hillshade_layer.dataProvider(), 1, 315, 45)
        hs_renderer.setZFactor(1.0)
        hillshade_layer.setRenderer(hs_renderer)

        # Set blend mode to multiply for overlay effect
        hillshade_layer.setBlendMode(6)  # 6 = Multiply blend mode
        hillshade_layer.setOpacity(0.5)

        # Add to project
        QgsProject.instance().addMapLayer(hillshade_layer, False)

        # Find and add to the Raster group
        root = QgsProject.instance().layerTreeRoot()
        main_group = root.findGroup("Movecost Results")
        if main_group:
            raster_group = main_group.findGroup("Raster")
            if raster_group:
                raster_group.insertLayer(0, hillshade_layer)

    layer.triggerRepaint()


def _apply_vector_style_to_layer(layer, styles_dir):
    """Apply appropriate style to a vector layer."""
    layer_name = layer.name().lower()
    geom_type = layer.geometryType()  # 0=Point, 1=Line, 2=Polygon

    # Determine style based on layer name and geometry type
    if geom_type == 1:  # Line
        if 'lcp' in layer_name:
            qml_path = os.path.join(styles_dir, 'lcp_style.qml')
        elif 'isoline' in layer_name:
            qml_path = os.path.join(styles_dir, 'isoline_style.qml')
        elif 'netw' in layer_name or 'network' in layer_name:
            qml_path = os.path.join(styles_dir, 'network_style.qml')
        else:
            qml_path = os.path.join(styles_dir, 'lcp_style.qml')
    elif geom_type == 0:  # Point
        if any(kw in layer_name for kw in ['cost', 'w_cost', 'dest']):
            qml_path = os.path.join(styles_dir, 'points_cost_style.qml')
        else:
            return  # No style for generic points
    elif geom_type == 2:  # Polygon
        qml_path = os.path.join(styles_dir, 'area_style.qml')
    else:
        return

    if os.path.exists(qml_path):
        layer.loadNamedStyle(qml_path)
        layer.triggerRepaint()
