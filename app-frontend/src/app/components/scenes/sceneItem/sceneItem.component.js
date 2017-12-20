// Component code
import sceneTpl from './sceneItem.html';

const rfSceneItem = {
    templateUrl: sceneTpl,
    transclude: true,
    controller: 'SceneItemController',
    bindings: {
        scene: '<',
        actions: '<',
        onView: '&',
        onDownload: '&',

        selected: '<?',
        onSelect: '&?',
        isDisabled: '<?',
        apiSource: '<?',
        planetKey: '<?',
        onPassPlanetThumbnail: '&?'
    }
};

export default rfSceneItem;
