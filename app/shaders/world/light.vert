#version 330 core

uniform mat4 uModel;
uniform mat4 uViewProjection;
uniform mat4 uModelViewProjection;
uniform mat4 uNormalMatrix;
uniform mat4 uInverseModel;
uniform vec3 uRepelPosition;

in      vec3 aPosition;
in      vec3 aNormal;
in      vec2 aUV;
in      vec3 aTangent;

out     vec3 vPosition;
out     vec3 vNormal;
out     vec2 vUV;


void main() {

    // Pass some variables to the fragment shader
    vec3 pos = vec3(uModel * vec4(aPosition, 1.0));

    vPosition = pos;
    vUV = aUV;

    // If scaled not uniformly, 
    // this will screw up ( i think ... )
    vNormal   = vec3(uNormalMatrix * vec4(aNormal, 0.0));

    gl_Position = uModelViewProjection * vec4( aPosition, 1.0);

}