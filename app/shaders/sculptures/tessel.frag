
#version 330 core


const float MAX_TRACE_DISTANCE = 2.;           // max trace distance
const float INTERSECTION_PRECISION = 0.0001;        // precision of the intersection
const int NUM_OF_TRACE_STEPS = 10;
const float PI  = 3.14159;

uniform float uTime;
uniform vec3 uDimensions;

in vec3 vPos;
in vec3 vEye;
in vec3 vNorm;

in vec3 vHand1;
in vec3 vHand2;
in vec3 vLight;

in vec2 vUV;

out vec4 color;

vec3 sPos[10];

/*

From https://www.shadertoy.com/view/XtSGDK

*/

float udRoundBox( vec3 p, vec3 b, float r )
{
  return length(max(abs(p)-b,0.0))-r;
}

vec3 deform( in vec3 p, in float time, out float sca )
{
    float s = 0.34*sqrt(dot(p*p,p*p));
    //float s = 1.0;

    p = p/s;

    p.xyz += 4.0*sin(vec3(0.0,2.0,4.0) + time);
    
    sca = s;
    
  return p;
}

float shape( vec3 p )
{
    vec3 q = mod( p+2.0, 4.0 ) -2.0;

    float d1 = udRoundBox(q,vec3(0.10,0.02,1.00),0.02);
    float d2 = udRoundBox(q,vec3(0.02,1.00,0.10),0.02);
    float d3 = udRoundBox(q,vec3(1.00,0.10,0.02),0.02);
    float d4 = udRoundBox(q,vec3(0.30,0.30,0.30),0.02);

    return min( min(d1,d2), min(d3,d4) );
}

float sdBox( vec3 p, vec3 b )
{
  vec3 d = abs(p) - b;
  return min(max(d.x,max(d.y,d.z)),0.0) +
         length(max(d,0.0));
}

float opU( float d1, float d2 ){
  return min(d1,d2);
}

vec2 opU( vec2 d1, vec2 d2 )
{
    return  d1.x < d2.x ? d1 : d2 ;
}

float opS( float d1, float d2 )
{
    return max(-d1,d2);
}

vec2 map( vec3 pos )
{

    vec2 res = vec2(-sdBox( pos  , vec3( uDimensions.x * 10.5 + INTERSECTION_PRECISION * 5. ) ) , 1. );

    float s = 10.0;
    vec3 p = pos;
    pos = deform( pos, uTime, s );

    res = opU( res , vec2( shape( pos ) * s , 2.));
    return res; 
}


float hash( float n ) { return fract(sin(n)*753.5453123); }
float noise( in vec3 x )
{
    vec3 p = floor(x);
    vec3 f = fract(x);
    f = f*f*(3.0-2.0*f);
  
    float n = p.x + p.y*157.0 + 113.0*p.z;
    return mix(mix(mix( hash(n+  0.0), hash(n+  1.0),f.x),
                   mix( hash(n+157.0), hash(n+158.0),f.x),f.y),
               mix(mix( hash(n+113.0), hash(n+114.0),f.x),
                   mix( hash(n+270.0), hash(n+271.0),f.x),f.y),f.z);
}





float fNoise( vec3 p ){
   
    float n;
    
    n += noise( p * 20. ) * .5;
    n += noise( p * 200. ) * .1;
    n += noise( p * 60. ) * .3;
    n += noise( p * 5. );
    
    return n;
   
    
}

vec2 mapN( vec3 pos )
{

    vec2 res = vec2( sdBox( pos  , vec3( .4 ) ) , 1. );
    res.x = opS( sdBox( pos  , vec3( .26  ) ) , res.x );

    float s = 10.0;
    vec3 p = pos;
    pos = deform( pos, uTime, s );

    res = opU( res , vec2( shape( pos ) * s , 2.));
    res -= .002 * fNoise( p * 10. );
    return res; 
}



vec2 calcIntersection( in vec3 ro, in vec3 rd ){

    float h =  INTERSECTION_PRECISION * 2.0;
    float t = 0.0;
    float res = -1.0;
    float id = -1.;
    
    for( int i=0; i < NUM_OF_TRACE_STEPS ; i++ ){
        
        if( h < INTERSECTION_PRECISION || t > MAX_TRACE_DISTANCE ) break;
        vec2 m = map( ro+rd*t );
        h = m.x;
        t += h;
        id = m.y;
        
    }

    if( t < MAX_TRACE_DISTANCE ) res = t;
    if( t > MAX_TRACE_DISTANCE ) id =-1.0;
    
    return vec2( res , id );
     
}

/*float softshadow( in vec3 ro, in vec3 rd, in float mint, in float tmax )
{
  float res = 1.0;
    float t = mint;
    for( int i=0; i<50; i++ )
    {
    float h = map( ro + rd*t );
        res = min( res, 20.*h/t );
        t += clamp( h, 0.02, 0.10 );
        if( h<0.001 || t>tmax ) break;
    }
    return clamp( res, 0.0, 1.0 );

}*/

//----------------------------------------------------------------------------------------
float shadow( in vec3 ro, in vec3 rd)
{
  float res = 1.0;
  float t = 0.05;
  float h;
  
  for (int i = 0; i < 8; i++)
  {
    h = map( ro + rd*t ).x;
    res = min(6.0*h / t, res);
    t += h;
  }
    return max(res, 0.0);
}


// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      map(pos+eps.xyy).x - map(pos-eps.xyy).x,
      map(pos+eps.yxy).x - map(pos-eps.yxy).x,
      map(pos+eps.yyx).x - map(pos-eps.yyx).x );

  return normalize(nor);
}


// Calculates the normal by taking a very small distance,
// remapping the function, and getting normal for that
vec3 calcNoiseNormal( in vec3 pos ){
    
  vec3 eps = vec3( 0.001, 0.0, 0.0 );
  vec3 nor = vec3(
      mapN(pos+eps.xyy).x - mapN(pos-eps.xyy).x,
      mapN(pos+eps.yxy).x - mapN(pos-eps.yxy).x,
      mapN(pos+eps.yyx).x - mapN(pos-eps.yyx).x );

  return normalize(nor);
}

void main(){

  vec3 ro = vPos;
  vec3 rd = normalize( vPos - vEye );


  vec3 handDir1 = normalize( vHand1 - ro);
  vec3 handDir2 = normalize( vHand2 - ro);

  vec2 res = calcIntersection( ro , rd );


  vec3 col = vec3( 0. );
  if( res.y > .5 ){

    vec3 pos = ro + rd * res.x;

    vec3 handDir1 = normalize( vHand1 - pos);
    vec3 handDir2 = normalize( vHand2 - pos);
    vec3 norm;

    
    norm = calcNormal( pos );

    col = norm * .5 + .5;

    vec3 lightDir = normalize( vLight - pos);

    vec3 refl = reflect( lightDir , norm );
    col = vec3(dot( normalize(lightDir) , norm ));//* .5 + .5;

    float spec = pow(max( 0. , dot( rd , refl )), 5.);
    float lamb = max( 0. , dot( lightDir , norm ));

    vec3 specC = vec3( .1 , 1.8 , 1. ) * spec;
    vec3 lambC = vec3( .1 , .6 , 1. ) * lamb;
    vec3  ambC = vec3( .0 , .2 , 0.4 );
    col = specC + lambC + ambC;//* .5 + .5;

    //col *=  norm * .5 + .5;

    float ss = shadow( ro , rd );

    //col -= ss * 30.;


  }


  if( abs(vUV.x - .5) > .49 ||  abs(vUV.y - .5) > .49 ){
    col /= 2.;//vec3(1.);
  }


  color = vec4( col , 1. );



}
