#include "camera.h"

Ray Camera::create_ray(float u, float v) {
    Vec3 h = this->height;
    Vec3 w = this->width;
    Vec3 ll = this->lower_left;
    Vec3 origin = this->origin;
    Vec3 direction = Vec3::normalize((ll + (v * h) + (u * w)) - origin);
    return Ray(origin, direction);
}
